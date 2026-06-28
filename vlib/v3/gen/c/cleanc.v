module c

import os
import strings
import v3.flat
import v3.types

// FlatGen emits flat gen output used by c.
pub struct FlatGen {
mut:
	sb                           strings.Builder
	indent                       int
	a                            &flat.FlatAst = unsafe { nil }
	used_fns                     map[string]bool
	used_fn_names                []string
	test_files                   map[string]bool
	str_lits                     []string
	str_lit_ids                  map[string]int
	global_types                 map[string]types.Type
	enum_vals                    map[string]int
	defers                       []flat.NodeId
	fn_defers                    []flat.NodeId
	fn_defer_counts              map[int]string
	defer_capture_names          []string
	defer_capture_types          map[string]types.Type
	interfaces                   map[string][]string
	const_vals                   map[string]flat.NodeId
	const_modules                map[string]string
	const_init_order             []string
	global_modules               map[string]string
	global_inits                 map[string]flat.NodeId // qualified global name -> initializer value node
	global_init_order            []string               // qualified global names, in declaration order
	iface_impls                  map[string][]string    // interface name -> implementing concrete type names
	iface_type_ids               map[string]int         // "${iface}::${concrete}" -> 1-based type id
	module_init_fns              []string               // C names of module-level `init()` fns, in source order
	module_init_fn_modules       map[string]string      // C init fn name -> V module name
	module_imports               map[string][]string    // module -> imported modules
	c_directives                 []CDirective
	c_flags                      []string
	libc_compat_fns              map[string]bool
	tc                           &types.TypeChecker = unsafe { nil }
	has_builtins                 bool
	has_stdatomic_header         bool
	has_stdatomic_compat_header  bool
	tmp_count                    int
	line_start                   bool
	field_name_set               map[string]bool   // every struct field's C name (lazy) — for const/field collision checks
	modules                      map[string]string // alias -> full module name
	fn_ptr_types                 map[string]string // fn_ptr:ret|params -> typedef name
	fixed_array_ret_wrappers     map[string]bool   // bare fixed-array c_type name -> has a return wrapper struct
	emitted_fixed_array_typedefs map[string]bool   // bare fixed-array typedefs already written (shared across passes)
	fn_decl_param_types          map[string][]types.Type
	fn_decl_ret_types            map[string]types.Type // fn decl name (and qualified variants) -> return type
	struct_decl_infos            map[string]StructDeclInfo
	struct_decl_short_infos      map[string]StructDeclInfo
	const_runtime_inits          []string
	const_runtime_init_modules   []string
	runtime_inits                []string
	runtime_init_modules         []string
	compiler_vroot               string
	c99_mode                     bool
	cur_fn_name                  string
	cur_param_names              []string
	cur_param_type_values        []types.Type
	cur_param_types              map[string]types.Type
	cur_fn_ret                   types.Type = types.Type(types.void_)
	cur_fn_ret_is_optional       bool
	cur_fn_ret_base              types.Type = types.Type(types.void_)
	// in_return is true only while generating a `return` statement's value, so a bare
	// generic literal (`return Box{...}`) may adopt `cur_fn_ret`'s concrete instance —
	// but a literal in a local decl / argument elsewhere in the body does not.
	in_return               bool
	expected_expr_type      types.Type = types.Type(types.void_)
	expected_enum           string
	needed_optional_types   map[string]string
	emitted_optional_types  map[string]bool
	emitted_fns             map[string]bool
	array_method_cache      map[string]string
	param_types_cache       map[string][]types.Type        // (name|fallback) -> resolved param types
	embedded_fields_by_type map[string][]types.StructField // type name -> its embedded fields (usually empty)
	param_types_by_short    map[string][]types.Type        // method short-name suffix -> param types (fallback index)
	spawn_wrapper_names     map[string]string
	spawn_wrapper_defs      []string
	callback_wrapper_names  map[string]string
	callback_wrapper_defs   []string
	parallel_used           bool
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

// new creates a FlatGen value for c.
pub fn FlatGen.new() FlatGen {
	return FlatGen{
		sb:                           strings.new_builder(4096)
		used_fns:                     map[string]bool{}
		test_files:                   map[string]bool{}
		str_lit_ids:                  map[string]int{}
		global_types:                 map[string]types.Type{}
		enum_vals:                    map[string]int{}
		interfaces:                   map[string][]string{}
		const_vals:                   map[string]flat.NodeId{}
		const_modules:                map[string]string{}
		const_init_order:             []string{}
		global_modules:               map[string]string{}
		global_inits:                 map[string]flat.NodeId{}
		global_init_order:            []string{}
		iface_impls:                  map[string][]string{}
		iface_type_ids:               map[string]int{}
		module_init_fns:              []string{}
		module_init_fn_modules:       map[string]string{}
		module_imports:               map[string][]string{}
		c_directives:                 []CDirective{}
		c_flags:                      []string{}
		libc_compat_fns:              map[string]bool{}
		modules:                      map[string]string{}
		fn_ptr_types:                 map[string]string{}
		fixed_array_ret_wrappers:     map[string]bool{}
		emitted_fixed_array_typedefs: map[string]bool{}
		fn_decl_param_types:          map[string][]types.Type{}
		fn_decl_ret_types:            map[string]types.Type{}
		struct_decl_infos:            map[string]StructDeclInfo{}
		struct_decl_short_infos:      map[string]StructDeclInfo{}
		cur_param_names:              []string{}
		cur_param_type_values:        []types.Type{}
		cur_param_types:              map[string]types.Type{}
		needed_optional_types:        map[string]string{}
		emitted_optional_types:       map[string]bool{}
		emitted_fns:                  map[string]bool{}
		array_method_cache:           map[string]string{}
		param_types_cache:            map[string][]types.Type{}
		embedded_fields_by_type:      map[string][]types.StructField{}
		param_types_by_short:         map[string][]types.Type{}
		spawn_wrapper_names:          map[string]string{}
		spawn_wrapper_defs:           []string{}
		callback_wrapper_names:       map[string]string{}
		callback_wrapper_defs:        []string{}
		str_lits:                     []string{}
		defers:                       []flat.NodeId{}
		fn_defers:                    []flat.NodeId{}
		fn_defer_counts:              map[int]string{}
		defer_capture_names:          []string{}
		defer_capture_types:          map[string]types.Type{}
		const_runtime_inits:          []string{}
		const_runtime_init_modules:   []string{}
		runtime_inits:                []string{}
		runtime_init_modules:         []string{}
		compiler_vroot:               ''
		line_start:                   true
	}
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
	g.str_lits = []string{}
	g.defers = []flat.NodeId{}
	g.fn_defers = []flat.NodeId{}
	g.fn_defer_counts = map[int]string{}
	g.defer_capture_names = []string{}
	g.defer_capture_types = map[string]types.Type{}
	g.const_runtime_inits = []string{}
	g.const_runtime_init_modules = []string{}
	g.runtime_inits = []string{}
	g.runtime_init_modules = []string{}
	g.compiler_vroot = ''
	g.str_lit_ids = map[string]int{}
	g.global_types = map[string]types.Type{}
	g.enum_vals = map[string]int{}
	g.interfaces = map[string][]string{}
	g.const_vals = map[string]flat.NodeId{}
	g.const_modules = map[string]string{}
	g.const_init_order = []string{}
	g.global_modules = map[string]string{}
	g.global_inits = map[string]flat.NodeId{}
	g.global_init_order = []string{}
	g.iface_impls = map[string][]string{}
	g.iface_type_ids = map[string]int{}
	g.module_init_fns = []string{}
	g.module_init_fn_modules = map[string]string{}
	g.module_imports = map[string][]string{}
	g.c_directives = []CDirective{}
	g.c_flags = []string{}
	g.libc_compat_fns = map[string]bool{}
	g.has_stdatomic_header = false
	g.has_stdatomic_compat_header = false
	g.modules = map[string]string{}
	g.fn_ptr_types = map[string]string{}
	g.fixed_array_ret_wrappers = map[string]bool{}
	g.emitted_fixed_array_typedefs = map[string]bool{}
	g.fn_decl_param_types = map[string][]types.Type{}
	g.fn_decl_ret_types = map[string]types.Type{}
	g.struct_decl_infos = map[string]StructDeclInfo{}
	g.struct_decl_short_infos = map[string]StructDeclInfo{}
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	g.needed_optional_types = map[string]string{}
	g.emitted_optional_types = map[string]bool{}
	g.emitted_fns = map[string]bool{}
	g.array_method_cache = map[string]string{}
	g.param_types_cache = map[string][]types.Type{}
	g.embedded_fields_by_type = map[string][]types.StructField{}
	g.param_types_by_short = map[string][]types.Type{}
	g.spawn_wrapper_names = map[string]string{}
	g.spawn_wrapper_defs = []string{}
	g.callback_wrapper_names = map[string]string{}
	g.callback_wrapper_defs = []string{}
	g.parallel_used = false
	g.tc = unsafe { tc }
	if g.tc.a == unsafe { nil } {
		g.tc.collect(a)
	}
	g.has_builtins = g.tc.has_builtins
	g.collect_gen_info()
	g.precompute_embedded_fields()
	g.precompute_param_type_index()
	g.collect_interface_impls()
	g.preseed_struct_fn_ptr_types()
	g.preseed_global_fn_ptr_types()
	// Decide fixed-array return wrappers before generating function bodies, so
	// signatures, returns and call sites all agree on the wrapped types.
	g.populate_fixed_array_ret_wrappers()
	const_code := g.precompute_consts()
	orig_sb := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(4096)
	g.line_start = true
	g.gen_fns_dispatch(no_parallel)
	fn_code := g.sb.str()
	// `.str()` copies out of the builder; free the emptied backing array under -gc none.
	unsafe { g.sb.free() }
	g.sb = orig_sb
	g.line_start = orig_line_start
	g.preamble()
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
	g.builtin_abi_decls()
	g.multi_return_typedefs()
	g.optional_typedefs()
	g.global_decls()
	g.forward_decls()
	g.enum_str_forward_decls()
	g.callback_wrapper_decls()
	g.spawn_wrapper_decls()
	g.register_interface_strings()
	g.string_literals()
	g.interface_method_stubs()
	g.enum_str_defs()
	g.sb.write_string(const_code)
	// The final builder now owns a copy of the const code.
	unsafe { const_code.free() }
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
	g.sb.write_string(fn_code)
	// The final builder now owns a copy of the function code.
	unsafe { fn_code.free() }
	result := g.sb.str()
	// Keep only the returned C string, not the builder's copied backing array.
	unsafe { g.sb.free() }
	return result
}

// node_kind_id supports node kind id handling for c.
fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

// collect_gen_info updates collect gen info state for c.
fn (mut g FlatGen) collect_gen_info() {
	mut cur_module := 'main'
	mut cur_file := ''
	mut seen_import_in_file := false
	for node_idx in 0 .. g.a.nodes.len {
		node := g.a.nodes[node_idx]
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
			mut ptypes := []types.Type{}
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			for i in 0 .. node.children_count {
				child := g.a.child_node(&node, i)
				if node_kind_id(child) == 75 {
					raw_pt := g.tc.parse_type(child.typ)
					pt := raw_pt
					ptypes << raw_pt
					if pt is types.FnType {
						ct := g.tc.c_type(raw_pt)
						g.resolve_fn_ptr_type(ct)
					}
				}
			}
			ptypes = g.fn_param_types_with_implicit_veb_ctx(node, ptypes)
			g.register_fn_decl_param_types(node.value, full_name, ptypes)
			g.register_fn_decl_ret_type(node.value, full_name, node.typ)
			// Module-level `init()` functions run once at startup. Collect their C
			// names so _vinit can invoke them (V semantics).
			if node.value == 'init' && ptypes.len == 0 {
				init_cname := qualified_fn_name_in_module(cur_module, 'init')
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
		if node.kind == .directive && node.value == 'flag' && node.typ.len > 0 {
			flag := c_flag_arg(node.typ, g.compiler_vroot, cur_file)
			if flag.len > 0 && flag !in g.c_flags {
				g.c_flags << flag
			}
			continue
		}
		if node.kind == .directive && node.value == 'pkgconfig' && node.typ.len > 0 {
			for flag in c_pkgconfig_flags(node.typ) {
				if flag.len > 0 && flag !in g.c_flags {
					g.c_flags << flag
				}
			}
			continue
		}
		if kind_id == 62 {
			full_name := qualify_name_in_module(cur_module, node.value)
			g.register_struct_decl_info(node.value, full_name, cur_module, node)
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
			is_flag := node.typ == 'flag'
			mut val := 0
			enum_name := qualify_name_in_module(cur_module, node.value)
			for i in 0 .. node.children_count {
				f := g.a.child_node(&node, i)
				if f.children_count > 0 {
					if enum_val := g.enum_field_expr_value(g.a.child(f, 0)) {
						val = enum_val
					}
				}
				if is_flag {
					g.enum_vals['${enum_name}.${f.value}'] = 1 << val
					val++
				} else {
					g.enum_vals['${enum_name}.${f.value}'] = val
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
					if (cur_module.len == 0 || cur_module == 'main' || cur_module == 'builtin')
						&& f.value !in g.const_vals {
						g.const_vals[f.value] = g.a.child(f, 0)
						g.const_modules[f.value] = cur_module
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
	g.modules['strings'] = 'strings'
	g.collect_const_init_order_from_files()
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
		if include_arg.contains('prealloc_atomics.h') || include_arg.contains('filelock_helpers.h') {
			return true
		}
		if is_stdatomic_header(include_arg) {
			g.has_stdatomic_header = true
		}
		if is_stdatomic_compat_header(include_arg) {
			g.has_stdatomic_compat_header = true
		}
		g.add_c_directive(module_name, '#include ${include_arg}', before_import)
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

fn is_stdatomic_header(include_arg string) bool {
	normalized := include_arg.replace('\\', '/')
	return normalized == '<stdatomic.h>' || normalized == '"stdatomic.h"'
		|| normalized.ends_with('/stdatomic.h"') || is_stdatomic_compat_header(normalized)
}

fn is_stdatomic_compat_header(include_arg string) bool {
	normalized := include_arg.replace('\\', '/')
	return normalized.contains('/thirdparty/stdatomic/') && normalized.ends_with('/atomic.h"')
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
	clean := raw.trim_space()
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
	for node in g.a.nodes {
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
		dep_module := startup_module_key(dep)
		g.visit_module_init(dep_module, module_to_init, mut visiting, mut visited, mut result)
	}
	visiting.delete(mod)
	visited[mod] = true
	if init_fn := module_to_init[mod] {
		result << init_fn
	}
}

fn (g &FlatGen) ordered_c_directives() []string {
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
		clean := directive.trim_space()
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
	body := text[1..].trim_space()
	if body.len == 0 {
		return ''
	}
	idx := body.index_u8(` `)
	if idx < 0 {
		return body
	}
	return body[..idx]
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
		return clean[..hash].trim_space()
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
	name := raw.trim_space()
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
	return result.output.trim_space().fields()
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
		'solaris', 'wasm32_emscripten']
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

// register_fn_decl_param_types updates register fn decl param types state for c.
fn (mut g FlatGen) register_fn_decl_param_types(name string, full_name string, ptypes []types.Type) {
	if name !in g.fn_decl_param_types {
		g.fn_decl_param_types[name] = ptypes.clone()
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		dotted_name := '${g.tc.cur_module}.${name}'
		if dotted_name !in g.fn_decl_param_types {
			g.fn_decl_param_types[dotted_name] = ptypes.clone()
		}
	}
	if full_name !in g.fn_decl_param_types {
		g.fn_decl_param_types[full_name] = ptypes.clone()
	}
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
	cname := c_name(name)
	if cname != name && cname !in g.fn_decl_ret_types {
		g.fn_decl_ret_types[cname] = rt
	}
}

// register_struct_decl_info updates register struct decl info state for c.
fn (mut g FlatGen) register_struct_decl_info(name string, full_name string, module_name string, node flat.Node) {
	info := StructDeclInfo{
		node:      node
		module:    module_name
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

fn (g &FlatGen) enum_selector_base_name(name string) ?string {
	if name in g.tc.enum_names || name in g.tc.flag_enums {
		return name
	}
	qname := g.tc.qualify_name(name)
	if qname in g.tc.enum_names || qname in g.tc.flag_enums {
		return qname
	}
	if name.contains('.') || g.tc.cur_file.len == 0 {
		return none
	}
	candidates := g.tc.file_selective_imports['${g.tc.cur_file}\n${name}'] or { return none }
	for candidate in candidates {
		if candidate in g.tc.enum_names || candidate in g.tc.flag_enums {
			return candidate
		}
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
		g.gen_expr(id)
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
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_map_str_expr(id flat.NodeId, typ types.Type) bool {
	clean := map_str_clean_type(typ)
	if clean !is types.Map {
		return false
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
	return true
}

fn map_str_clean_type(typ types.Type) types.Type {
	clean := types.unwrap_pointer(typ)
	if clean is types.Alias {
		return clean.base_type
	}
	return clean
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

// gen_expr_with_expected_type emits expr with expected type output for c.
fn (mut g FlatGen) gen_expr_with_expected_type(id flat.NodeId, expected types.Type) {
	old_expected := g.expected_expr_type
	old_expected_enum := g.expected_enum
	g.expected_expr_type = expected
	if expected is types.Enum {
		g.expected_enum = expected.name
	}
	node := g.a.nodes[int(id)]
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
	if expected !is types.Pointer && expected !is types.Void && actual is types.Pointer
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
	if g.gen_interface_value_expr(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_sum_value_expr(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	g.gen_expr(id)
	g.expected_expr_type = old_expected
	g.expected_enum = old_expected_enum
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
	actual0 := raw_actual0
	actual_type := if actual0 is types.Alias { actual0.base_type } else { actual0 }
	if actual_type is types.SumType {
		return false
	}
	sum_name := g.resolve_sum_name(sum_type.name)
	variant := g.resolve_variant(sum_name, actual_type.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	if variant !in variants {
		return false
	}
	ct := g.tc.c_type(sum_type0)
	idx := g.sum_type_index(sum_name, variant)
	field := g.sum_field_name(variant)
	if g.variant_references_sum(variant, sum_name) {
		inner_ct := g.tc.c_type(g.tc.parse_type(variant))
		g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup((${inner_ct}[]){')
		g.gen_expr(id)
		g.write('}, sizeof(${inner_ct}))}')
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
	if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			return param_type
		}
		if param_type := g.cur_param_types[node.value] {
			return param_type
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
		inner_ct := g.tc.c_type(variant_type)
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
				g.write('.${c_name(sf.value)} = ')
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
				g.write('.${c_name(sf.value)} = ')
				g.gen_expr(g.a.child(sf, 0))
			}
			g.write('}, sizeof(${inner_ct}))}')
		} else {
			g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup((${inner_ct}[]){')
			g.gen_expr(inner_id)
			g.write('}, sizeof(${inner_ct}))}')
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
	if child.value in g.cur_param_types {
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
	if expected is types.Enum {
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
		return c_name(type_name)
	}
	if type_name.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(type_name)
	}
	t := g.tc.parse_type(type_name)
	return g.tc.c_type(t)
}

fn (mut g FlatGen) sizeof_target(value string) string {
	if value.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(value)
	}
	if fixed_target := c_fixed_array_typedef_sizeof_target(value) {
		return fixed_target
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
			if mod.len == 0 || mod == g.tc.cur_module
				|| (g.tc.cur_module in ['', 'main', 'builtin'] && mod in ['', 'main', 'builtin']) {
				return g.const_primary_name(name)
			}
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

// const_ref_name_from_node converts const ref name from node data for c.
fn (g &FlatGen) const_ref_name_from_node(node flat.Node) string {
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
				if mod := g.const_modules[const_name] {
					g.tc.cur_module = mod
				}
				dep_expr := g.const_expr_to_string(g.const_vals[const_name], next_seen)
				g.tc.cur_module = old_module
				if dep_expr.trim_space().len > 0 {
					return dep_expr
				}
			}
			g.expr_to_string(id)
		}
		.infix {
			lhs := g.const_expr_to_string(g.a.child(&node, 0), seen)
			rhs := g.const_expr_to_string(g.a.child(&node, 1), seen)
			'(${lhs}) ${g.op_str(node.op)} (${rhs})'
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
				inner_ct := g.tc.c_type(g.tc.parse_type(variant_name))
				payload := if inner_val.trim_space().len == 0 { '0' } else { inner_val }
				return '(${ct}){.typ = ${idx}, .${field} = (${inner_ct}[]){${payload}}}'
			}
			if target_type !is types.Primitive && target_type !is types.Char
				&& target_type !is types.Rune && target_type !is types.ISize
				&& target_type !is types.USize && target_type !is types.Pointer
				&& target_type !is types.Enum {
				return g.expr_to_string(id)
			}
			child0 := g.const_expr_to_string(g.a.child(&node, 0), seen)
			child := if child0.trim_space().len == 0 { '0' } else { child0 }
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
						if const_val.trim_space().len > 0 {
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
						inner_ct := g.tc.c_type(g.tc.parse_type(variant))
						const_val := g.const_expr_to_string(val_id, seen)
						payload := if const_val.trim_space().len > 0 {
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
							if const_val.trim_space().len > 0 {
								const_val
							} else {
								g.expr_to_string_with_expected_type(val_id, ftyp)
							}
						}
					} else {
						const_val := g.const_expr_to_string(val_id, seen)
						if const_val.trim_space().len > 0 {
							const_val
						} else {
							g.expr_to_string(val_id)
						}
					}
					if field.value.len == 0 {
						parts << val
					} else {
						parts << '.${c_name(field.value)} = ${val}'
					}
				} else {
					parts << g.const_expr_to_string(g.a.child(&node, i), seen)
				}
			}
			'(${ct}){${parts.join(', ')}}'
		}
		.string_literal {
			'{"${c_escape(node.value)}", ${node.value.len}, 1}'
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .enum_val, .sizeof_expr {
			g.expr_to_string(id)
		}
		.offsetof_expr {
			ct := g.sizeof_target(node.value)
			'offsetof(${ct}, ${c_name(node.typ)})'
		}
		else {
			g.expr_to_string(id)
		}
	}
}

// const_ident_c_name converts const ident c name data for c.
fn (g &FlatGen) const_ident_c_name(name string) string {
	if name.contains('.') {
		return c_name(name)
	}
	mod := if name in g.const_modules { g.const_modules[name] } else { '' }
	if mod.len > 0 && mod != 'main' && mod != 'builtin' {
		return c_name('${mod}.${name}')
	}
	if (mod == '' || mod == 'main') && name in g.const_modules {
		return c_name('main.${name}')
	}
	return c_name(name)
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
	return g.fixed_array_len_value(arr).trim_space() == '0'
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
		if expr.trim_space().len > 0 {
			return expr
		}
		return g.const_ident_c_name(const_name)
	}
	return c_name(raw_len)
}

fn (mut g FlatGen) fixed_array_decl_parts(arr types.ArrayFixed) (string, string) {
	len_expr := g.fixed_array_len_value(arr)
	if arr.elem_type is types.ArrayFixed {
		base_ct, suffix := g.fixed_array_decl_parts(arr.elem_type)
		return base_ct, '[${len_expr}]${suffix}'
	}
	return g.tc.c_type(arr.elem_type), '[${len_expr}]'
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
				g.write('"${cv}"')
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
				g.write("'${v}'")
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
					g.write(c_name('${mod}.${node.value}'))
				} else {
					g.write(c_name(node.value))
				}
			} else {
				g.write(c_name(node.value))
			}
		}
		.enum_val {
			if node.value in g.enum_vals {
				eval := g.enum_vals[node.value]
				g.write('${eval}')
				return
			}
			if node.typ.len > 0 {
				short_name := node.value.trim_left('.').all_after_last('.')
				if eval := g.enum_value_for_type(node.typ, short_name) {
					g.write('${eval}')
					return
				}
			}
			if g.expected_enum.len > 0 {
				ekey := '${g.expected_enum}.${node.value}'
				if ekey in g.enum_vals {
					eval := g.enum_vals[ekey]
					g.write('${eval}')
					return
				}
				if !g.expected_enum.contains('.') && g.tc.cur_module.len > 0
					&& g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
					qkey := '${g.tc.cur_module}.${g.expected_enum}.${node.value}'
					if qkey in g.enum_vals {
						eval := g.enum_vals[qkey]
						g.write('${eval}')
						return
					}
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
			// A call to a fixed-array-returning function yields the wrapper struct;
			// unwrap `.ret_arr` so the result behaves as the array value everywhere
			// (indexing, arg passing, memcpy into a destination).
			ret_t := g.declared_call_return_type(id)
			if ret_t is types.ArrayFixed && g.tc.c_type(ret_t) in g.fixed_array_ret_wrappers {
				g.write('(')
				g.gen_call(id, node)
				g.write(').ret_arr')
			} else {
				g.gen_call(id, node)
			}
		}
		.spawn_expr {
			g.gen_spawn_expr(node)
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
				lhs_node := g.a.nodes[int(lhs_id)]
				rhs_node := g.a.nodes[int(rhs_id)]
				if lhs_node.kind == .infix && !infix_can_skip_child_parens(node.op, lhs_node.op) {
					g.write('(')
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
					g.write(')')
				} else {
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
				}
				g.write(' ${g.op_str(node.op)} ')
				if rhs_node.kind == .infix && !infix_can_skip_child_parens(node.op, rhs_node.op) {
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
			if node.op == .arrow {
				child_type := g.usable_expr_type(child_id)
				if child_type is types.Channel {
					elem_ct := g.tc.c_type(child_type.elem_type)
					tmp := g.tmp_name()
					g.write('({${elem_ct} ${tmp} = (${elem_ct}){0}; sync__Channel__pop(')
					g.gen_expr(child_id)
					g.write(', &${tmp}); ${tmp};})')
					return
				}
			}
			if node.op == .mul && child.kind == .ident {
				if typ := g.current_param_type(child.value) {
					if typ !is types.Pointer {
						g.gen_expr(child_id)
						return
					}
				} else if typ := g.cur_param_types[child.value] {
					if typ !is types.Pointer {
						g.gen_expr(child_id)
						return
					}
				}
			}
			if node.op == .amp && g.gen_amp_c_string_literal(child_id, child) {
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
						g.write(g.op_str(node.op))
						g.gen_expr(child_id)
					}
				} else {
					g.write(g.op_str(node.op))
					g.gen_expr(child_id)
				}
			} else {
				g.write(g.op_str(node.op))
				g.gen_expr(child_id)
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
				c_key := g.tc.c_type(clean_rhs.key_type)
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
			} else {
				panic('internal error: non-map membership reached C backend in ${g.cur_fn_name}: rhs=${rhs_type.name()} kind=${rhs.kind} value=${rhs.value}')
			}
		}
		.postfix {
			g.gen_expr(g.a.child(&node, 0))
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
			base_type0 := g.tc.resolve_type(base_id)
			if base_type0 is types.Channel && node.value in ['closed', 'len'] {
				if node.value == 'closed' {
					g.write('(atomic_load_u16(&')
					g.gen_expr(base_id)
					g.write('->closed) != 0)')
				} else {
					g.write('sync__Channel__len(')
					g.gen_expr(base_id)
					g.write(')')
				}
				return
			}
			base_is_local := if base.kind == .ident {
				(g.tc.cur_scope.lookup(base.value) or { types.Type(types.void_) }) !is types.Void
			} else {
				false
			}
			// A method used as a value (e.g. `game.draw` passed as a callback) rather
			// than a field access — bind the receiver and yield a wrapper function.
			if g.gen_method_value_closure(base_id, base_type0, node.value) {
				return
			}
			enum_selector_qbase := if base.kind == .ident && base.value != 'C' && !base_is_local {
				g.enum_selector_base_name(base.value) or { '' }
			} else {
				''
			}
			if base.kind == .ident && base.value == 'C' {
				g.write(node.value)
			} else if enum_selector_qbase.len > 0 {
				ekey := '${enum_selector_qbase}.${node.value}'
				if eval := g.enum_vals[ekey] {
					g.write('${eval}')
				} else {
					g.write('0')
				}
			} else if base_type0 is types.String && node.value == 'len' {
				g.gen_expr(base_id)
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
			} else if base.kind == .call && base.children_count == 2
				&& g.c_typedef_cast_call_name(base).len > 0 {
				cast_name := g.c_typedef_cast_call_name(base)
				cast_arg_id := g.a.child(&base, 1)
				g.write('((${c_name(cast_name)}*)')
				g.gen_expr(cast_arg_id)
				g.write(')->${c_name(node.value)}')
			} else if base.kind == .cast_expr && base.children_count > 0
				&& (base.value.starts_with('C.') || base.value.contains('__')) {
				cast_child_id := g.a.child(&base, 0)
				cast_name := if base.value.starts_with('C.') { base.value[2..] } else { base.value }
				g.write('((${c_name(cast_name)}*)')
				g.gen_expr(cast_child_id)
				g.write(')->${c_name(node.value)}')
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
				g.write(c_name(node.value))
			} else if node.value == 'len' && base.kind == .ident {
				base_type := g.tc.resolve_type(base_id)
				if base_type is types.ArrayFixed {
					g.write(g.fixed_array_len_value(base_type))
				} else {
					raw_type := g.tc.cur_scope.lookup(base.value) or { base_type }
					g.gen_expr(base_id)
					if raw_type is types.Pointer {
						g.write('->len')
					} else {
						g.write('.len')
					}
				}
			} else if base.kind == .ident && !base_is_local && g.has_import_alias(base.value) {
				mod := g.import_alias_module(base.value) or { '' }
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
					g.write(c_name(full_qname))
				} else {
					g.write(c_name('${short_mod}.${node.value}'))
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
					if ekey in g.enum_vals {
						eval := g.enum_vals[ekey]
						g.write('${eval}')
					} else if ekey2 in g.enum_vals {
						eval := g.enum_vals[ekey2]
						g.write('${eval}')
					} else {
						g.write(c_name('${qname}.${node.value}'))
					}
				} else {
					g.write(c_name('${qname}.${node.value}'))
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
				g.write(c_name(embedded.name))
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
				for embedded in embedded_path {
					op := if is_ptr { '->' } else { '.' }
					g.write('${op}${c_name(embedded.name)}')
					is_ptr = embedded.typ is types.Pointer
				}
				final_op := if is_ptr { '->' } else { '.' }
				g.write('${final_op}${c_name(node.value)}')
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
				g.write(c_name(node.value))
			}
		}
		.index {
			base_id := g.a.child(&node, 0)
			base_type := g.tc.resolve_type(base_id)
			if node.value == 'range' {
				g.gen_slice_expr(node, base_id, base_type)
			} else if base_type is types.Map {
				c_key := g.value_c_type(base_type.key_type)
				c_val := g.value_c_type(base_type.value_type)
				g.write('(*(${c_val}*)map__get(&')
				g.gen_expr(base_id)
				g.write(', &(${c_key}[]){')
				g.gen_expr(g.a.child(&node, 1))
				g.write('}, &(${c_val}[]){0}))')
			} else {
				is_fixed_array_index, fixed_is_ptr, _ := fixed_array_index_info(base_type)
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
					is_array_index, is_ptr, arr_type := array_index_info(base_type)
					if is_array_index {
						index_type := if g.expected_expr_type is types.OptionType
							|| g.expected_expr_type is types.ResultType
							|| g.expected_expr_is_optional_struct() {
							g.expected_expr_type
						} else if node.typ.starts_with('?') || node.typ.starts_with('!') {
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
			if node.value in g.interfaces || g.tc.qualify_name(node.value) in g.interfaces {
				g.write('(${ct}){0}')
			} else if target_type is types.SumType {
				g.gen_sum_cast_expr(target_type, g.a.child(&node, 0))
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
			ct := g.optional_type_name(g.optional_none_type(id))
			g.write('(${ct}){.ok = false}')
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
				} else if last.kind == .if_expr {
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
			clean := types.unwrap_pointer(expr_type)
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
			} else {
				g.write('1')
			}
		}
		.as_expr {
			expr_id := g.a.child(&node, 0)
			expr_type := g.tc.resolve_type(expr_id)
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
			} else {
				g.gen_expr(expr_id)
			}
		}
		.sizeof_expr {
			g.write('sizeof(${g.sizeof_target(node.value)})')
		}
		.offsetof_expr {
			ct := g.type_name_c_type(node.value)
			g.write('offsetof(${ct}, ${c_name(node.typ)})')
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
	lhs_arr := array_like_type(types.unwrap_pointer(lhs_type)) or { return false }
	rhs_arr := array_like_type(types.unwrap_pointer(rhs_type)) or { return false }
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
		g.write(', sizeof(${g.tc.c_type(elem_type)})')
	}
	g.write(')')
	return true
}

fn (mut g FlatGen) gen_array_value_arg(id flat.NodeId, typ types.Type) {
	if typ is types.Pointer {
		g.write('*')
	}
	g.gen_expr(id)
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
	g.c99_feature_test_macros()
	g.writeln('#include <stdio.h>')
	g.writeln('#include <stdlib.h>')
	g.writeln('#include <string.h>')
	g.writeln('#include <stddef.h>')
	g.writeln('#include <float.h>')
	g.writeln('#include <stdint.h>') // guarantees UINTPTR_MAX for the pointer-width atomic helpers
	g.writeln('#include <math.h>')
	g.writeln('#include <unistd.h>')
	g.c99_atomic_compat_decls()
	if g.has_builtins {
		g.writeln('#include <time.h>')
		g.writeln('#include <sys/time.h>')
		g.writeln('#include <errno.h>')
		g.writeln('#include <signal.h>')
		g.writeln('#include <execinfo.h>')
		g.writeln('#include <dirent.h>')
		g.writeln('#include <sys/stat.h>')
		g.writeln('#include <fcntl.h>')
		g.writeln('#include <sys/ioctl.h>')
		g.writeln('#include <sys/utsname.h>')
		g.writeln('#include <pthread.h>')
		g.writeln('#include <semaphore.h>')
		g.writeln('#include <termios.h>')
		g.writeln('#include <unistd.h>')
		g.writeln('#include <arpa/inet.h>')
		g.writeln('#include <netdb.h>')
		g.writeln('#include <netinet/in.h>')
		g.writeln('#include <netinet/tcp.h>')
		g.writeln('#include <sys/socket.h>')
		g.writeln('#ifdef __APPLE__')
		g.writeln('#define panic mach_panic')
		g.writeln('#include <mach/mach.h>')
		g.writeln('#include <mach/task.h>')
		g.writeln('#include <mach/mach_time.h>')
		g.writeln('#include <mach-o/dyld.h>')
		g.writeln('#undef panic')
		g.writeln('#ifndef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP')
		g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
		g.writeln('#define pthread_rwlockattr_setkind_np(attr, kind) 0')
		g.writeln('#endif')
		g.writeln('#endif')
	}
	if g.libc_compat_fns['gettid'] {
		g.writeln('#ifdef __linux__')
		g.writeln('#include <sys/syscall.h>')
		g.writeln('#endif')
	}
	for directive in g.ordered_c_directives() {
		g.writeln(directive)
	}
	g.writeln('')
	g.writeln('typedef signed char i8;')
	g.writeln('typedef short i16;')
	g.writeln('typedef int i32;')
	g.writeln('typedef long long i64;')
	g.writeln('typedef unsigned char u8;')
	g.writeln('typedef unsigned char byte;')
	g.writeln('typedef unsigned short u16;')
	g.writeln('typedef unsigned int u32;')
	g.writeln('typedef unsigned long long u64;')
	g.writeln('#ifndef __bool_true_false_are_defined')
	g.writeln('typedef int bool;')
	g.writeln('#endif')
	g.writeln('typedef void* voidptr;')
	g.writeln('typedef int int_literal;')
	g.writeln('typedef double float_literal;')
	g.writeln('struct sync__Channel;')
	g.writeln('typedef struct sync__Channel* chan;')
	g.writeln('#define true 1')
	g.writeln('#define false 0')
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

fn (mut g FlatGen) c99_atomic_compat_decls() {
	if g.has_stdatomic_header {
		return
	}
	g.writeln('typedef volatile uintptr_t atomic_uintptr_t;')
	g.writeln('#ifndef memory_order_relaxed')
	g.writeln('#define memory_order_relaxed 0')
	g.writeln('#define memory_order_consume 1')
	g.writeln('#define memory_order_acquire 2')
	g.writeln('#define memory_order_release 3')
	g.writeln('#define memory_order_acq_rel 4')
	g.writeln('#define memory_order_seq_cst 5')
	g.writeln('#endif')
	g.writeln('#ifndef atomic_thread_fence')
	g.writeln('#define atomic_thread_fence(order) __sync_synchronize()')
	g.writeln('#endif')
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
		g.writeln('#define SYS_gettid __NR_gettid')
		g.writeln('#endif')
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
	if g.has_stdatomic_compat_header {
		return
	}
	// Atomic helpers. We use compiler __atomic_* builtins (memory order 5 == __ATOMIC_SEQ_CST).
	// clang/gcc inline the generic _n / RMW builtins. tcc only implements the inline
	// __atomic_{add,sub,fetch}_* RMW builtins; for load/store/exchange/cas it has no generic
	// _n form, so we route those to the sized __atomic_*_N libcalls (resolved from libc).
	g.writeln('static inline u32 atomic_fetch_add_u32(void* ptr, u32 delta) { return __atomic_fetch_add((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_add_u64(void* ptr, u64 delta) { return __atomic_fetch_add((u64*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_sub_u64(void* ptr, u64 delta) { return __atomic_fetch_sub((u64*)ptr, delta, 5); }')
	g.writeln('static inline byte atomic_load_byte(void* ptr) { return __atomic_fetch_add((byte*)ptr, 0, 5); }')
	g.writeln('static inline u16 atomic_load_u16(void* ptr) { return __atomic_fetch_add((u16*)ptr, 0, 5); }')
	g.writeln('static inline u32 atomic_load_u32(void* ptr) { return __atomic_fetch_add((u32*)ptr, 0, 5); }')
	g.writeln('static inline u64 atomic_load_u64(void* ptr) { return __atomic_fetch_add((u64*)ptr, 0, 5); }')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return *(void* volatile*)ptr; }')
	g.writeln('#ifdef __TINYC__')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_1((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_2((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_4((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_8((u64*)ptr, val, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_4((u32*)ptr, (u32)(size_t)val, 5); }')
	g.writeln('#else')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_8((u64*)ptr, (u64)(size_t)val, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_2((u16*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_4((u32*)ptr, expected, desired, 5, 5); }')
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
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_n((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_n((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_n((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_n((u64*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_n((void**)ptr, val, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_n((void**)ptr, (void**)expected, (void*)desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_n((byte*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_n((u64*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return atomic_compare_exchange_strong_ptr(ptr, expected, desired); }')
	g.writeln('static inline void cpu_relax(void) { __asm__ __volatile__("" ::: "memory"); }')
}

fn (mut g FlatGen) builtin_abi_decls() {
	if !g.has_builtins {
		return
	}
	g.libc_compat_decls()
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
	// Weak fallbacks for the heap-tracking hooks. A program that provides real
	// implementations (e.g. a `vheap_alloc`/`vheap_free` from a linked C file, as
	// some projects do) overrides these without a redefinition/static-vs-non-static
	// clash against that file's own non-static prototype.
	g.writeln('__attribute__((weak)) void vheap_alloc(void* p, u64 n) { (void)p; (void)n; }')
	g.writeln('__attribute__((weak)) void vheap_free(void* p) { (void)p; }')
	g.filelock_compat_decls()
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
	g.writeln('string rune__str(i32 c);')
	g.writeln('u8* malloc_noscan(ptrdiff_t n);')
	g.writeln('static inline string v3_c_lit(const char* s, int len) { return (string){.str = (u8*)s, .len = len, .is_lit = 1}; }')
	g.writeln('static inline string v3_char_string(int c) { return rune__str((i32)c); }')
	g.writeln('static inline string v3_f64_fixed(double x, int precision) { char tmp[128]; int n = snprintf(tmp, sizeof(tmp), "%.*f", precision, x); if (n < 0) return v3_c_lit("", 0); if (n < (int)sizeof(tmp)) { u8* out = malloc_noscan(n + 1); memcpy(out, tmp, n + 1); return (string){.str = out, .len = n, .is_lit = 0}; } u8* out = malloc_noscan(n + 1); snprintf((char*)out, (size_t)n + 1, "%.*f", precision, x); return (string){.str = out, .len = n, .is_lit = 0}; }')
	g.writeln('static inline string v3_int_zpad(int n, int width) { string s = int__str(n); if (n < 0) return s; while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln('static inline string v3_i64_zpad(i64 n, int width) { string s = i64__str(n); if (n < 0) return s; while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln('static inline string v3_u64_zpad(u64 n, int width) { string s = u64__str(n); while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln('static inline i64 v3_map_signed(void* p, int bytes) { if (bytes == 1) return *(signed char*)p; if (bytes == 2) return *(short*)p; if (bytes == 8) return *(long long*)p; return *(int*)p; }')
	g.writeln('static inline u64 v3_map_unsigned(void* p, int bytes) { if (bytes == 1) return *(unsigned char*)p; if (bytes == 2) return *(unsigned short*)p; if (bytes == 8) return *(unsigned long long*)p; return *(unsigned int*)p; }')
	g.writeln('static inline string v3_f32_array_str(float* vals, int n) { string out = v3_c_lit("[", 1); for (int i = 0; i < n; ++i) { if (i > 0) out = string__plus(out, v3_c_lit(", ", 2)); out = string__plus(out, f64__str((double)vals[i])); } return string__plus(out, v3_c_lit("]", 1)); }')
	g.writeln('static inline string v3_f64_array_str(double* vals, int n) { string out = v3_c_lit("[", 1); for (int i = 0; i < n; ++i) { if (i > 0) out = string__plus(out, v3_c_lit(", ", 2)); out = string__plus(out, f64__str(vals[i])); } return string__plus(out, v3_c_lit("]", 1)); }')
	g.writeln('static inline string v3_map_str_piece(void* p, int kind, int bytes, int fixed_len) {')
	g.writeln('\tif (kind == 1) { return string__plus(string__plus(v3_c_lit("\'", 1), *(string*)p), v3_c_lit("\'", 1)); }')
	g.writeln('\tif (kind == 2) { return v3_i64_zpad(v3_map_signed(p, bytes), 0); }')
	g.writeln('\tif (kind == 3) { return u64__str(v3_map_unsigned(p, bytes)); }')
	g.writeln('\tif (kind == 4) { i32 r = bytes == 1 ? (i32)(*(u8*)p) : *(i32*)p; return string__plus(string__plus(v3_c_lit("`", 1), rune__str(r)), v3_c_lit("`", 1)); }')
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
	g.writeln('static inline bool array_eq_array(Array a, Array b, int depth) { if (a.len != b.len) return false; Array* ad = (Array*)a.data; Array* bd = (Array*)b.data; for (int i = 0; i < a.len; i++) { if (depth > 1) { if (!array_eq_array(ad[i], bd[i], depth - 1)) return false; } else if (ad[i].element_size == sizeof(string)) { if (!array_eq_string(ad[i], bd[i])) return false; } else if (!array_eq_raw(ad[i], bd[i], ad[i].element_size)) return false; } return true; }')
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

fn (mut g FlatGen) filelock_compat_decls() {
	if !g.libc_compat_fns['filelock'] {
		return
	}
	g.writeln('#ifndef V_OS_FILELOCK_HELPERS_H')
	g.writeln('#define V_OS_FILELOCK_HELPERS_H')
	g.writeln('#ifdef _WIN32')
	g.writeln('#include <windows.h>')
	g.writeln('static int v_filelock_lock(HANDLE handle, int exclusive, int immediate, unsigned long long start, unsigned long long len) {')
	g.writeln('\tOVERLAPPED overlap;')
	g.writeln('\tmemset(&overlap, 0, sizeof(overlap));')
	g.writeln('\toverlap.Offset = (DWORD)(start & 0xffffffffULL);')
	g.writeln('\toverlap.OffsetHigh = (DWORD)(start >> 32);')
	g.writeln('\tDWORD flags = immediate ? LOCKFILE_FAIL_IMMEDIATELY : 0;')
	g.writeln('\tif (exclusive) { flags |= LOCKFILE_EXCLUSIVE_LOCK; }')
	g.writeln('\tDWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL);')
	g.writeln('\tDWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32);')
	g.writeln('\treturn LockFileEx(handle, flags, 0, low, high, &overlap) ? 0 : -1;')
	g.writeln('}')
	g.writeln('static int v_filelock_unlock(HANDLE handle, unsigned long long start, unsigned long long len) {')
	g.writeln('\tOVERLAPPED overlap;')
	g.writeln('\tmemset(&overlap, 0, sizeof(overlap));')
	g.writeln('\toverlap.Offset = (DWORD)(start & 0xffffffffULL);')
	g.writeln('\toverlap.OffsetHigh = (DWORD)(start >> 32);')
	g.writeln('\tDWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL);')
	g.writeln('\tDWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32);')
	g.writeln('\treturn UnlockFileEx(handle, 0, low, high, &overlap) ? 0 : -1;')
	g.writeln('}')
	g.writeln('#else')
	g.writeln('#include <fcntl.h>')
	g.writeln('#include <unistd.h>')
	g.writeln('static int v_filelock_lock(int fd, int exclusive, int immediate, unsigned long long start, unsigned long long len) {')
	g.writeln('\tstruct flock fl;')
	g.writeln('\tmemset(&fl, 0, sizeof(fl));')
	g.writeln('\tfl.l_type = exclusive ? F_WRLCK : F_RDLCK;')
	g.writeln('\tfl.l_whence = SEEK_SET;')
	g.writeln('\tfl.l_start = (off_t)start;')
	g.writeln('\tfl.l_len = len == 0 ? 0 : (off_t)len;')
	g.writeln('\treturn fcntl(fd, immediate ? F_SETLK : F_SETLKW, &fl);')
	g.writeln('}')
	g.writeln('static int v_filelock_unlock(int fd, unsigned long long start, unsigned long long len) {')
	g.writeln('\tstruct flock fl;')
	g.writeln('\tmemset(&fl, 0, sizeof(fl));')
	g.writeln('\tfl.l_type = F_UNLCK;')
	g.writeln('\tfl.l_whence = SEEK_SET;')
	g.writeln('\tfl.l_start = (off_t)start;')
	g.writeln('\tfl.l_len = len == 0 ? 0 : (off_t)len;')
	g.writeln('\treturn fcntl(fd, F_SETLK, &fl);')
	g.writeln('}')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) collect_fixed_array_typedefs_needed() map[string]FixedArrayTypedefInfo {
	mut needed := map[string]FixedArrayTypedefInfo{}
	old_module := g.tc.cur_module
	for name, ret_type in g.tc.fn_ret_types {
		g.tc.cur_module = module_from_qualified_name(name)
		g.collect_fixed_array_typedef(ret_type, mut needed)
	}
	for name, param_types in g.tc.fn_param_types {
		g.tc.cur_module = module_from_qualified_name(name)
		for param_type in param_types {
			g.collect_fixed_array_typedef(param_type, mut needed)
		}
	}
	for name, fields in g.tc.structs {
		g.tc.cur_module = g.fixed_array_typedef_type_module(name, old_module)
		for field in fields {
			g.collect_fixed_array_typedef(field.typ, mut needed)
		}
	}
	for name, fields in g.tc.interface_fields {
		g.tc.cur_module = module_from_qualified_name(name)
		for field in fields {
			g.collect_fixed_array_typedef(field.typ, mut needed)
		}
	}
	for name, typ in g.global_types {
		g.tc.cur_module = g.global_modules[name] or { old_module }
		g.collect_fixed_array_typedef(typ, mut needed)
	}
	for _, typ in g.tc.c_globals {
		g.tc.cur_module = old_module
		g.collect_fixed_array_typedef(typ, mut needed)
	}
	for name, typ in g.tc.const_types {
		g.tc.cur_module = g.const_modules[name] or { old_module }
		g.collect_fixed_array_typedef(typ, mut needed)
	}
	g.tc.cur_module = old_module
	for node in g.a.nodes {
		g.collect_fixed_array_typedef_text(node.typ, mut needed)
		match node.kind {
			.array_init, .array_literal, .cast_expr, .sizeof_expr, .typeof_expr {
				g.collect_fixed_array_typedef_text(node.value, mut needed)
			}
			else {}
		}
	}
	g.tc.cur_module = old_module
	return needed
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
	needed := g.collect_fixed_array_typedefs_needed()
	for name, _ in needed {
		g.fixed_array_ret_wrappers[name] = true
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
	elem_ct := g.tc.c_type(arr.elem_type)
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
	if t is types.ArrayFixed {
		bare := g.tc.c_type(t)
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

fn (mut g FlatGen) collect_fixed_array_typedef(typ types.Type, mut needed map[string]FixedArrayTypedefInfo) {
	if typ is types.ArrayFixed {
		name := g.tc.c_type(typ)
		existing_priority := if name in needed {
			fixed_array_typedef_module_priority(needed[name].module)
		} else {
			-1
		}
		current_priority := fixed_array_typedef_module_priority(g.tc.cur_module)
		if name !in needed || current_priority > existing_priority {
			needed[name] = FixedArrayTypedefInfo{
				arr:    typ
				module: g.tc.cur_module
			}
		}
		g.collect_fixed_array_typedef(typ.elem_type, mut needed)
	} else if typ is types.Pointer {
		g.collect_fixed_array_typedef(typ.base_type, mut needed)
	} else if typ is types.Alias {
		g.collect_fixed_array_typedef(typ.base_type, mut needed)
	} else if typ is types.OptionType {
		g.collect_fixed_array_typedef(typ.base_type, mut needed)
	} else if typ is types.ResultType {
		g.collect_fixed_array_typedef(typ.base_type, mut needed)
	} else if typ is types.Array {
		g.collect_fixed_array_typedef(typ.elem_type, mut needed)
	} else if typ is types.Map {
		g.collect_fixed_array_typedef(typ.key_type, mut needed)
		g.collect_fixed_array_typedef(typ.value_type, mut needed)
	} else if typ is types.FnType {
		for param in typ.params {
			g.collect_fixed_array_typedef(param, mut needed)
		}
		g.collect_fixed_array_typedef(typ.return_type, mut needed)
	} else if typ is types.MultiReturn {
		for item in typ.types {
			g.collect_fixed_array_typedef(item, mut needed)
		}
	}
}

fn (mut g FlatGen) collect_fixed_array_typedef_text(type_text string, mut needed map[string]FixedArrayTypedefInfo) {
	clean := type_text.trim_space()
	if clean.len == 0 {
		return
	}
	typ := g.tc.parse_type(clean)
	if fixed_array_typedef_has_non_decimal_len(typ) {
		return
	}
	g.collect_fixed_array_typedef(typ, mut needed)
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
	if arr.elem_type is types.ArrayFixed {
		inner_name := g.tc.c_type(arr.elem_type)
		if inner := needed[inner_name] {
			g.emit_fixed_array_typedef(inner_name, inner, needed, mut emitted)
		}
	}
	elem_ct := g.tc.c_type(arr.elem_type)
	len_expr := g.fixed_array_len_value(arr)
	g.writeln('typedef ${elem_ct} ${name}[${len_expr}];')
	g.tc.cur_module = old_module
	emitted[name] = true
}

fn (mut g FlatGen) global_decls() {
	old_module := g.tc.cur_module
	for name, typ in g.global_types {
		if mod := g.global_modules[name] {
			g.tc.cur_module = mod
		} else {
			g.tc.cur_module = old_module
		}
		if typ is types.ArrayFixed {
			c_elem, dims := g.fixed_array_decl_parts(typ)
			init := if g.has_zero_sized_leading_init_slot(typ) { '' } else { ' = {0}' }
			g.writeln('${c_elem} ${c_name(name)}${dims}${init};')
			continue
		}
		mut ct := g.tc.c_type(typ)
		if ct == 'void' {
			continue
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if name.starts_with('C.') {
			continue
		}
		init := if g.can_use_global_brace_zero_init(typ, ct) { ' = {0}' } else { '' }
		g.writeln('${ct} ${c_name(name)}${init};')
	}
	g.tc.cur_module = old_module
	if g.global_types.len > 0 {
		g.writeln('')
	}
	g.emit_global_inits()
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
	for qname in g.global_init_order {
		val_id := g.global_inits[qname] or { continue }
		if int(val_id) < 0 {
			continue
		}
		// g_main_argc/g_main_argv are filled in by main's preamble (from argc/argv)
		// *before* _vinit runs, and are zero by default in C anyway. Re-emitting their
		// `= 0` initializer here would clobber the real argv, leaving os.args empty.
		cqname := c_name(qname)
		if cqname == 'g_main_argc' || cqname == 'g_main_argv' {
			continue
		}
		if mod := g.global_modules[qname] {
			g.tc.cur_module = mod
		} else {
			g.tc.cur_module = old_module
		}
		if typ := g.global_types[qname] {
			if typ is types.ArrayFixed {
				target := c_name(qname)
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
		if expr_str.trim_space().len == 0 {
			continue
		}
		target := c_name(qname)
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
	g.const_collect_deps(val_id, mut deps)
	return deps
}

fn (g &FlatGen) const_collect_deps(val_id flat.NodeId, mut deps []string) {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(val_id)]
	if node.kind == .ident || node.kind == .selector {
		const_name := g.const_ref_name_from_node(node)
		if const_name.len > 0 {
			deps << const_name
		}
	}
	for i in 0 .. node.children_count {
		g.const_collect_deps(g.a.child(&node, i), mut deps)
	}
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
	if name in g.const_modules {
		g.tc.cur_module = g.const_modules[name]
	}
	val_node := g.a.nodes[int(val_id)]
	if val_node.kind == .empty {
		g.tc.cur_module = old_module
		return
	}
	v_type := if val_node.kind == .offsetof_expr {
		types.Type(types.usize_)
	} else {
		g.tc.resolve_type(val_id)
	}
	ct := g.tc.c_type(v_type)
	qname := g.const_ident_c_name(name)
	expr_str := if v_type is types.Array && val_node.kind == .array_literal {
		g.expr_to_string_with_expected_type(val_id, v_type)
	} else if g.is_const_expr(val_id) {
		g.const_expr_to_string(val_id, []string{})
	} else {
		g.expr_to_string(val_id)
	}
	if expr_str.trim_space().len == 0 {
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
	c_key := g.tc.c_type(map_type.key_type)
	c_val := g.tc.c_type(map_type.value_type)
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
	c_key := g.tc.c_type(map_type.key_type)
	c_val := g.tc.c_type(map_type.value_type)
	for i := 0; i + 1 < node.children_count; i += 2 {
		key := g.expr_to_string_with_expected_type(g.a.child(&node, i), map_type.key_type)
		val := g.expr_to_string_with_expected_type(g.a.child(&node, i + 1), map_type.value_type)
		g.queue_const_runtime_init('\tmap__set(&${target}, &(${c_key}[]){${key}}, &(${c_val}[]){${val}});')
	}
}

fn (mut g FlatGen) queue_fixed_array_runtime_init(target string, val_id flat.NodeId, fixed types.ArrayFixed) bool {
	expr := g.fixed_array_compound_literal_expr(val_id, fixed)
	if expr.trim_space().len == 0 {
		return false
	}
	g.queue_runtime_init('\tmemmove(${target}, ${expr}, sizeof(${target}));')
	return true
}

fn (mut g FlatGen) queue_const_fixed_array_runtime_init(target string, val_id flat.NodeId, fixed types.ArrayFixed) bool {
	expr := g.fixed_array_compound_literal_expr(val_id, fixed)
	if expr.trim_space().len == 0 {
		return false
	}
	g.queue_const_runtime_init('\tmemmove(${target}, ${expr}, sizeof(${target}));')
	return true
}

fn (mut g FlatGen) fixed_array_compound_literal_expr(val_id flat.NodeId, fixed types.ArrayFixed) string {
	init := g.fixed_array_initializer_string(val_id, fixed)
	if init.trim_space().len == 0 {
		return ''
	}
	c_elem, dims := g.fixed_array_decl_parts(fixed)
	return '(${c_elem}${dims})${init}'
}

fn (mut g FlatGen) fixed_array_initializer_string(val_id flat.NodeId, fixed types.ArrayFixed) string {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return ''
	}
	node := g.a.nodes[int(val_id)]
	if node.kind == .postfix && node.children_count > 0 {
		return g.fixed_array_initializer_string(g.a.child(&node, 0), fixed)
	}
	if node.kind == .array_init && node.children_count == 0 {
		return '{0}'
	}
	if node.kind != .array_literal {
		return ''
	}
	mut parts := []string{}
	for i in 0 .. node.children_count {
		child_id := g.a.child(&node, i)
		if fixed.elem_type is types.ArrayFixed {
			parts << g.fixed_array_initializer_string(child_id, fixed.elem_type)
		} else {
			parts << g.fixed_array_elem_initializer_string(child_id, fixed.elem_type)
		}
	}
	return '{${parts.join(', ')}}'
}

fn (mut g FlatGen) fixed_array_elem_initializer_string(val_id flat.NodeId, elem_type types.Type) string {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return '0'
	}
	node := g.a.nodes[int(val_id)]
	if g.is_const_expr(val_id) && !(node.kind == .prefix && node.op == .amp) {
		const_val := g.const_expr_to_string(val_id, []string{})
		if const_val.trim_space().len > 0 {
			return const_val
		}
	}
	expr := g.expr_to_string_with_expected_type(val_id, elem_type)
	if expr.trim_space().len > 0 {
		return expr
	}
	return '0'
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
		dep_module := startup_module_key(dep)
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
