module c

import os
import strings
import v3.flat
import v3.types

pub struct FlatGen {
mut:
	sb                      strings.Builder
	indent                  int
	a                       &flat.FlatAst = unsafe { nil }
	used_fns                map[string]bool
	used_fn_names           []string
	str_lits                []string
	str_lit_ids             map[string]int
	global_types            map[string]types.Type
	enum_vals               map[string]int
	defers                  []flat.NodeId
	fn_defers               []flat.NodeId
	fn_defer_counts         map[int]string
	defer_capture_names     []string
	defer_capture_types     map[string]types.Type
	interfaces              map[string][]string
	const_vals              map[string]flat.NodeId
	const_modules           map[string]string
	const_init_order        []string
	global_modules          map[string]string
	global_inits            map[string]flat.NodeId // qualified global name -> initializer value node
	global_init_order       []string               // qualified global names, in declaration order
	iface_impls             map[string][]string    // interface name -> implementing concrete type names
	iface_type_ids          map[string]int         // "${iface}::${concrete}" -> 1-based type id
	module_init_fns         []string               // C names of module-level `init()` fns, in source order
	module_init_fn_modules  map[string]string      // C init fn name -> V module name
	module_imports          map[string][]string    // module -> imported modules
	tc                      &types.TypeChecker = unsafe { nil }
	has_builtins            bool
	tmp_count               int
	line_start              bool
	modules                 map[string]string // alias -> full module name
	fn_ptr_types            map[string]string // fn_ptr:ret|params -> typedef name
	fn_decl_param_types     map[string][]types.Type
	struct_decl_infos       map[string]StructDeclInfo
	struct_decl_short_infos map[string]StructDeclInfo
	runtime_inits           []string
	compiler_vroot          string
	cur_fn_name             string
	cur_param_names         []string
	cur_param_type_values   []types.Type
	cur_param_types         map[string]types.Type
	cur_fn_ret              types.Type = types.Type(types.void_)
	cur_fn_ret_is_optional  bool
	cur_fn_ret_base         types.Type = types.Type(types.void_)
	expected_expr_type      types.Type = types.Type(types.void_)
	expected_enum           string
	needed_optional_types   map[string]string
	emitted_optional_types  map[string]bool
	emitted_fns             map[string]bool
	array_method_cache      map[string]string
	parallel_used           bool
}

// was_parallel reports whether the last fn codegen actually ran across threads.
pub fn (g &FlatGen) was_parallel() bool {
	return g.parallel_used
}

pub fn FlatGen.new() FlatGen {
	return FlatGen{
		sb:                      strings.new_builder(4096)
		used_fns:                map[string]bool{}
		str_lit_ids:             map[string]int{}
		global_types:            map[string]types.Type{}
		enum_vals:               map[string]int{}
		interfaces:              map[string][]string{}
		const_vals:              map[string]flat.NodeId{}
		const_modules:           map[string]string{}
		const_init_order:        []string{}
		global_modules:          map[string]string{}
		global_inits:            map[string]flat.NodeId{}
		global_init_order:       []string{}
		iface_impls:             map[string][]string{}
		iface_type_ids:          map[string]int{}
		module_init_fns:         []string{}
		module_init_fn_modules:  map[string]string{}
		module_imports:          map[string][]string{}
		modules:                 map[string]string{}
		fn_ptr_types:            map[string]string{}
		fn_decl_param_types:     map[string][]types.Type{}
		struct_decl_infos:       map[string]StructDeclInfo{}
		struct_decl_short_infos: map[string]StructDeclInfo{}
		cur_param_names:         []string{}
		cur_param_type_values:   []types.Type{}
		cur_param_types:         map[string]types.Type{}
		needed_optional_types:   map[string]string{}
		emitted_optional_types:  map[string]bool{}
		emitted_fns:             map[string]bool{}
		array_method_cache:      map[string]string{}
		str_lits:                []string{}
		defers:                  []flat.NodeId{}
		fn_defers:               []flat.NodeId{}
		fn_defer_counts:         map[int]string{}
		defer_capture_names:     []string{}
		defer_capture_types:     map[string]types.Type{}
		runtime_inits:           []string{}
		compiler_vroot:          ''
		line_start:              true
	}
}

pub fn (mut g FlatGen) gen(a &flat.FlatAst) string {
	tc := types.TypeChecker.new(a)
	return g.gen_with_used(a, map[string]bool{}, &tc)
}

pub fn (mut g FlatGen) gen_with_used(a &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker) string {
	return g.gen_with_used_options(a, used_fns, tc, false)
}

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
	g.runtime_inits = []string{}
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
	g.modules = map[string]string{}
	g.fn_ptr_types = map[string]string{}
	g.fn_decl_param_types = map[string][]types.Type{}
	g.struct_decl_infos = map[string]StructDeclInfo{}
	g.struct_decl_short_infos = map[string]StructDeclInfo{}
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types = map[string]types.Type{}
	g.needed_optional_types = map[string]string{}
	g.emitted_optional_types = map[string]bool{}
	g.emitted_fns = map[string]bool{}
	g.array_method_cache = map[string]string{}
	g.parallel_used = false
	g.tc = unsafe { tc }
	if g.tc.a == unsafe { nil } {
		g.tc.collect(a)
	}
	g.has_builtins = g.tc.has_builtins
	g.collect_gen_info()
	g.collect_interface_impls()
	g.preseed_struct_fn_ptr_types()
	g.preseed_global_fn_ptr_types()
	const_code := g.precompute_consts()
	orig_sb := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(4096)
	g.line_start = true
	g.gen_fns_dispatch(no_parallel)
	fn_code := g.sb.str()
	g.sb = orig_sb
	g.line_start = orig_line_start
	g.preamble()
	g.enum_decls()
	g.type_alias_decls()
	g.type_forward_decls()
	g.c_value_struct_stub_decls()
	g.fn_ptr_typedefs()
	g.struct_decls()
	g.builtin_compat_decls()
	g.multi_return_typedefs()
	g.optional_typedefs()
	g.late_compat_decls()
	g.global_decls()
	g.forward_decls()
	g.register_interface_strings()
	g.string_literals()
	g.interface_method_stubs()
	g.sb.write_string(const_code)
	if g.runtime_inits.len > 0 || g.module_init_fns.len > 0 || g.global_inits.len > 0 {
		g.writeln('void _vinit() {')
		for ri in g.runtime_inits {
			g.writeln(ri)
		}
		// Module-level init() functions run after const/global initialization.
		for init_fn in g.ordered_module_init_fns() {
			g.writeln('\t${init_fn}();')
		}
		g.writeln('}')
		g.writeln('')
	}
	g.sb.write_string(fn_code)
	result := g.sb.str()
	return result
}

fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

fn (mut g FlatGen) collect_gen_info() {
	mut cur_module := ''
	for node_idx in 0 .. g.a.nodes.len {
		node := g.a.nodes[node_idx]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			g.note_compiler_source_file(node.value)
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 61 {
			full_name := qualify_name_in_module(cur_module, node.value)
			mut ptypes := []types.Type{}
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
		if kind_id == 62 {
			full_name := qualify_name_in_module(cur_module, node.value)
			g.register_struct_decl_info(node.value, full_name, cur_module, node)
			continue
		}
		if kind_id == 64 {
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
					if f.value !in g.const_vals {
						g.const_vals[f.value] = g.a.child(f, 0)
						g.const_modules[f.value] = cur_module
					}
				}
			}
			continue
		}
		if kind_id == 72 {
			alias := node.typ.clone()
			mod_name := node.value.clone()
			if alias.len > 0 && mod_name.len > 0 {
				g.modules[alias] = mod_name
			}
			if cur_module.len > 0 && mod_name.len > 0 {
				dep_module := if mod_name.contains('.') {
					mod_name.all_after_last('.')
				} else {
					mod_name
				}
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
	}
}

fn (mut g FlatGen) collect_const_init_order_from_files() {
	mut seen := map[string]bool{}
	g.const_init_order = []string{}
	for node in g.a.nodes {
		if node_kind_id(node) != 77 || node.children_count == 0 {
			continue
		}
		mut cur_module := ''
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

fn (g &FlatGen) ordered_module_init_fns() []string {
	mut module_to_init := map[string]string{}
	for init_fn in g.module_init_fns {
		mod := g.module_init_fn_modules[init_fn] or { '' }
		module_to_init[mod] = init_fn
	}
	mut result := []string{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for init_fn in g.module_init_fns {
		mod := g.module_init_fn_modules[init_fn] or { '' }
		g.visit_module_init(mod, module_to_init, mut visiting, mut visited, mut result)
	}
	return result
}

fn (g &FlatGen) visit_module_init(mod string, module_to_init map[string]string, mut visiting map[string]bool, mut visited map[string]bool, mut result []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	for dep in g.module_imports[mod] or { []string{} } {
		g.visit_module_init(dep, module_to_init, mut visiting, mut visited, mut result)
	}
	visiting.delete(mod)
	visited[mod] = true
	if init_fn := module_to_init[mod] {
		result << init_fn
	}
}

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

fn (mut g FlatGen) gen_expr_with_expected_type(id flat.NodeId, expected types.Type) {
	old_expected := g.expected_expr_type
	old_expected_enum := g.expected_enum
	g.expected_expr_type = expected
	if expected is types.Enum {
		g.expected_enum = expected.name
	}
	node := g.a.nodes[int(id)]
	mut actual := g.usable_expr_type(id)
	if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			actual = param_type
		}
	}
	if node.kind == .ident {
		if _ := fn_type_from(expected) {
			call_name := g.call_key(id, node.value)
			if call_name in g.tc.fn_param_types || call_name in g.tc.fn_ret_types {
				g.write(c_name(call_name))
				g.expected_expr_type = old_expected
				g.expected_enum = old_expected_enum
				return
			}
		}
	}
	if expected is types.Array && node.kind == .array_literal {
		elem_type := if node.children_count > 0 {
			g.tc.resolve_type(g.a.child(&node, 0))
		} else {
			expected.elem_type
		}
		g.gen_array_literal_value(node, elem_type)
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

fn (mut g FlatGen) gen_sum_value_expr(id flat.NodeId, expected types.Type) bool {
	sum_type0 := if expected is types.Alias { expected.base_type } else { expected }
	if sum_type0 !is types.SumType {
		return false
	}
	sum_type := sum_type0 as types.SumType
	raw_actual0 := g.tc.resolve_type(id)
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
	actual0 := g.usable_expr_type(id)
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

fn (mut g FlatGen) gen_sum_cast_expr(target_type types.SumType, inner_id flat.NodeId) {
	inner := g.a.nodes[int(inner_id)]
	actual_type := g.tc.resolve_type(inner_id)
	actual_clean := types.unwrap_pointer(actual_type)
	variant_name0 := if inner.kind == .struct_init || inner.kind == .cast_expr {
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

fn (g &FlatGen) valid_node_id(id flat.NodeId) bool {
	return g.a != unsafe { nil } && int(id) >= 0 && int(id) < g.a.nodes.len
}

fn (g &FlatGen) const_storage_name(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('.') {
		return '${module_name}.${name}'
	}
	return name
}

fn (g &FlatGen) const_primary_name(name string) string {
	mod := if name in g.const_modules { g.const_modules[name] } else { '' }
	qname := g.const_storage_name(mod, name)
	if qname != name && qname in g.const_vals {
		return qname
	}
	return name
}

fn (g &FlatGen) is_const_alias_name(name string) bool {
	return g.const_primary_name(name) != name
}

fn (g &FlatGen) const_ref_name(name string) string {
	if !name.contains('.') && !name.contains('__') {
		cur_qname := g.const_storage_name(g.tc.cur_module, name)
		if cur_qname in g.const_vals {
			return cur_qname
		}
		if name in g.const_vals {
			return g.const_primary_name(name)
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
				dep_expr := g.const_expr_to_string(g.const_vals[const_name], next_seen)
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
				variant_name0 := if inner.kind == .struct_init || inner.kind == .cast_expr {
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
		else {
			g.expr_to_string(id)
		}
	}
}

fn (g &FlatGen) const_ident_c_name(name string) string {
	if name.contains('.') {
		return c_name(name)
	}
	mod := if name in g.const_modules { g.const_modules[name] } else { '' }
	if mod.len > 0 && mod != 'main' && mod != 'builtin' {
		return c_name('${mod}.${name}')
	}
	return c_name(name)
}

fn (mut g FlatGen) fixed_array_len_expr(type_name string, fallback int) string {
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

fn (mut g FlatGen) fixed_array_len_value(arr types.ArrayFixed) string {
	return g.fixed_array_len_raw(arr.len_expr, arr.len)
}

fn (mut g FlatGen) fixed_array_len_is_zero(arr types.ArrayFixed) bool {
	if value := g.tc.fixed_array_len_value(arr) {
		return value == 0
	}
	return g.fixed_array_len_value(arr).trim_space() == '0'
}

fn (mut g FlatGen) fixed_array_len_raw(raw_len string, fallback int) string {
	if raw_len.len == 0 {
		return '${fallback}'
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
				g.write(v)
			}
		}
		.string_literal {
			sid := g.intern_string(node.value)
			g.write('_str_${sid}')
		}
		.string_interp {
			g.gen_string_interp(node)
		}
		.ident {
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
			g.gen_call(id, node)
		}
		.infix {
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
				if lhs_node.kind == .infix {
					g.write('(')
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
					g.write(')')
				} else {
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
				}
				g.write(' ${g.op_str(node.op)} ')
				if rhs_node.kind == .infix {
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
			if node.op == .amp && child.kind == .struct_init {
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
			if base_type0 is types.Channel && node.value == 'closed' {
				g.write('(atomic_load_u16(&')
				g.gen_expr(base_id)
				g.write('->closed) != 0)')
				return
			}
			base_is_local := if base.kind == .ident {
				(g.tc.cur_scope.lookup(base.value) or { types.Type(types.void_) }) !is types.Void
			} else {
				false
			}
			if base.kind == .ident && base.value == 'C' {
				g.write(node.value)
			} else if base.kind == .ident && !base_is_local && (base.value in g.tc.enum_names
				|| g.tc.qualify_name(base.value) in g.tc.enum_names) {
				qbase := if base.value in g.tc.enum_names {
					base.value
				} else {
					g.tc.qualify_name(base.value)
				}
				ekey := '${qbase}.${node.value}'
				if eval := g.enum_vals[ekey] {
					g.write('${eval}')
				} else {
					g.write('0')
				}
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
			} else if base.kind == .ident && !base_is_local && base.value in g.modules {
				mod := g.modules[base.value]
				short_mod := if mod.contains('.') {
					mod.all_after_last('.')
				} else {
					mod
				}
				g.write(c_name('${short_mod}.${node.value}'))
			} else if base.kind == .selector && base.children_count > 0
				&& g.is_module_qualified_enum(base) {
				inner_base := g.a.child_node(&base, 0)
				mod := g.modules[inner_base.value]
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
					g.gen_expr(base_id)
					g.write('.str[')
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
		.array_init {
			raw_init_type := g.tc.parse_type(node.value)
			init_type := raw_init_type
			if init_type is types.ArrayFixed {
				ct := g.tc.c_type(raw_init_type)
				g.write('(${ct}){0}')
			} else {
				c_elem := g.tc.c_type(init_type)
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
			if _ := g.tc.cur_scope.lookup(node.value) {
				g.write('sizeof(${c_name(node.value)})')
			} else {
				t := g.tc.parse_type(node.value)
				ct := g.tc.c_type(t)
				g.write('sizeof(${ct})')
			}
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
			g.gen_expr(lhs_id)
			g.write(', ')
			g.gen_expr(rhs_id)
			g.write(')')
		}
		.eq {
			g.write('string__eq(')
			g.gen_expr(lhs_id)
			g.write(', ')
			g.gen_expr(rhs_id)
			g.write(')')
		}
		.ne {
			g.write('!string__eq(')
			g.gen_expr(lhs_id)
			g.write(', ')
			g.gen_expr(rhs_id)
			g.write(')')
		}
		.lt {
			g.write('string__lt(')
			g.gen_expr(lhs_id)
			g.write(', ')
			g.gen_expr(rhs_id)
			g.write(')')
		}
		.gt {
			g.write('string__lt(')
			g.gen_expr(rhs_id)
			g.write(', ')
			g.gen_expr(lhs_id)
			g.write(')')
		}
		.le {
			g.write('!string__lt(')
			g.gen_expr(rhs_id)
			g.write(', ')
			g.gen_expr(lhs_id)
			g.write(')')
		}
		.ge {
			g.write('!string__lt(')
			g.gen_expr(lhs_id)
			g.write(', ')
			g.gen_expr(rhs_id)
			g.write(')')
		}
		else {
			return false
		}
	}

	return true
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
	if inner_base.kind != .ident || inner_base.value !in g.modules {
		return false
	}
	mod := g.modules[inner_base.value]
	short_mod := if mod.contains('.') { mod.all_after_last('.') } else { mod }
	qname := '${short_mod}.${base.value}'
	return qname in g.tc.enum_names || base.value in g.tc.enum_names
}

fn (mut g FlatGen) preamble() {
	g.writeln('#include <stdio.h>')
	g.writeln('#include <stdlib.h>')
	g.writeln('#include <string.h>')
	g.writeln('#include <stddef.h>')
	g.writeln('#include <float.h>')
	g.writeln('#include <math.h>')
	g.writeln('#include <unistd.h>')
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
		g.writeln('#include <stdatomic.h>')
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

fn (mut g FlatGen) builtin_compat_decls() {
	if !g.has_builtins {
		return
	}
	g.writeln('#define array_new(elem_size, len, cap) __new_array((len), (cap), (elem_size))')
	g.writeln('#define array_push array__push')
	g.writeln('void array__push_many(array* a, void* val, int size);')
	g.writeln('static inline void array_push_many(Array* a, Array b) { array__push_many(a, b.data, b.len); }')
	g.writeln('#define array_push_many_ptr(a, val, size) array__push_many((a), (void*)(val), (size))')
	g.writeln('#define array_get array__get')
	g.writeln('#define array_set(a, i, ...) array__set(&(a), (i), __VA_ARGS__)')
	g.writeln('array array__clone(array* a);')
	g.writeln('static inline array array_clone(array a) { return array__clone(&a); }')
	g.writeln('#define array_slice array__slice')
	g.writeln('#define array_delete array__delete')
	g.writeln('#define array_ensure_cap array__ensure_cap')
	g.writeln('#define map__get_or_set map__get_and_set')
	g.writeln('void panic(string s);')
	g.writeln('bool u8__is_letter(u8 c);')
	g.writeln('bool u8__is_capital(u8 c);')
	g.writeln('bool string__is_capital(string s);')
	g.writeln('string string__to_lower_ascii(string s);')
	g.writeln('i32 rune__to_lower(i32 c);')
	g.writeln('string data_to_hex_string(u8* data, int len);')
	g.writeln('#ifndef V_COMMIT_HASH')
	g.writeln('#define V_COMMIT_HASH ""')
	g.writeln('#endif')
	g.writeln('static inline void vheap_alloc(void* p, u64 n) { (void)p; (void)n; }')
	g.writeln('static inline void vheap_free(void* p) { (void)p; }')
	g.writeln('static inline int v_prealloc_atomic_add_i32(int *ptr, int delta) { return __sync_add_and_fetch(ptr, delta); }')
	g.writeln('static inline int v_prealloc_atomic_load_i32(int *ptr) { return __sync_add_and_fetch(ptr, 0); }')
	g.writeln('static inline int v_prealloc_atomic_store_i32(int *ptr, int val) { return __sync_lock_test_and_set(ptr, val); }')
	g.writeln('static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) { return __sync_bool_compare_and_swap(ptr, expected, desired); }')
	g.writeln('static inline u32 atomic_fetch_add_u32(void* ptr, u32 delta) { return __sync_fetch_and_add((u32*)ptr, delta); }')
	g.writeln('static inline u64 atomic_fetch_add_u64(void* ptr, u64 delta) { return __sync_fetch_and_add((u64*)ptr, delta); }')
	g.writeln('static inline u64 atomic_fetch_sub_u64(void* ptr, u64 delta) { return __sync_fetch_and_sub((u64*)ptr, delta); }')
	g.writeln('static inline byte atomic_load_byte(void* ptr) { return __sync_fetch_and_add((byte*)ptr, 0); }')
	g.writeln('static inline u16 atomic_load_u16(void* ptr) { return __sync_fetch_and_add((u16*)ptr, 0); }')
	g.writeln('static inline u32 atomic_load_u32(void* ptr) { return __sync_fetch_and_add((u32*)ptr, 0); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __sync_lock_test_and_set((u64*)ptr, val); }')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __sync_lock_test_and_set((byte*)ptr, val); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __sync_lock_test_and_set((u16*)ptr, val); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __sync_lock_test_and_set((u32*)ptr, val); }')
	g.writeln('static inline u64 atomic_load_u64(void* ptr) { return __sync_fetch_and_add((u64*)ptr, 0); }')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return *(void* volatile*)ptr; }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __sync_lock_test_and_set((void**)ptr, val); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { u16 old = *expected; bool ok = __sync_bool_compare_and_swap((u16*)ptr, old, desired); if (!ok) { *expected = atomic_load_u16(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { u32 old = *expected; bool ok = __sync_bool_compare_and_swap((u32*)ptr, old, desired); if (!ok) { *expected = atomic_load_u32(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { void* old = *(void**)expected; bool ok = __sync_bool_compare_and_swap((void**)ptr, old, (void*)desired); if (!ok) { *(void**)expected = atomic_load_ptr(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { byte old = *expected; bool ok = __sync_bool_compare_and_swap((byte*)ptr, old, desired); if (!ok) { *expected = atomic_load_byte(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { u16 old = *expected; bool ok = __sync_bool_compare_and_swap((u16*)ptr, old, desired); if (!ok) { *expected = atomic_load_u16(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { u32 old = *expected; bool ok = __sync_bool_compare_and_swap((u32*)ptr, old, desired); if (!ok) { *expected = atomic_load_u32(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { u64 old = *expected; bool ok = __sync_bool_compare_and_swap((u64*)ptr, old, desired); if (!ok) { *expected = atomic_load_u64(ptr); } return ok; }')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return atomic_compare_exchange_strong_ptr(ptr, expected, desired); }')
	g.writeln('static inline void cpu_relax(void) { __asm__ __volatile__("" ::: "memory"); }')
	g.writeln('static inline double math__abs(double a) { return a < 0 ? -a : a; }')
	g.writeln('static inline double math__min(double a, double b) { return a < b ? a : b; }')
	g.writeln('static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};')
	g.writeln('static inline u64 _wymix(u64 a, u64 b) { __uint128_t r = ((__uint128_t)a) * b; return ((u64)r) ^ ((u64)(r >> 64)); }')
	g.writeln('static inline u64 wyhash64(u64 a, u64 b) { a ^= _wyp[0]; b ^= _wyp[1]; a *= 0xa0761d6478bd642full; b *= 0xe7037ed1a0b428dbull; return (a ^ (a >> 32)) ^ (b ^ (b >> 32)); }')
	g.writeln('static inline u64 wyhash(const void* key, size_t len, u64 seed, const u64* secret) { const unsigned char* p = (const unsigned char*)key; u64 h = seed ^ secret[0] ^ (u64)len; for (size_t i = 0; i < len; i++) h = wyhash64(h ^ (u64)p[i], secret[(i + 1) & 3]); return h; }')
	g.writeln('#ifndef _WIN32')
	g.writeln('static inline int v_filelock_lock(int fd, int exclusive, int immediate, unsigned long long start, unsigned long long len) { struct flock fl; memset(&fl, 0, sizeof(fl)); fl.l_type = exclusive ? F_WRLCK : F_RDLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)start; fl.l_len = len == 0 ? 0 : (off_t)len; return fcntl(fd, immediate ? F_SETLK : F_SETLKW, &fl); }')
	g.writeln('static inline int v_filelock_unlock(int fd, unsigned long long start, unsigned long long len) { struct flock fl; memset(&fl, 0, sizeof(fl)); fl.l_type = F_UNLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)start; fl.l_len = len == 0 ? 0 : (off_t)len; return fcntl(fd, F_SETLK, &fl); }')
	g.writeln('#endif')
	g.writeln('#define v_signal_with_handler_cast(sig, handler) signal((sig), ((void (*)(int))(handler)))')
	g.writeln('string string__clone(string a);')
	g.writeln('void string__free(string* s);')
	g.writeln('static inline u64 v3_map_hash_bytes(const void* data, int len) { const unsigned char* p = (const unsigned char*)data; u64 h = 1469598103934665603ULL; for (int i = 0; i < len; i++) { h ^= (u64)p[i]; h *= 1099511628211ULL; } return h; }')
	g.writeln('static inline u64 v3_map_hash_string(void* pkey) { string* s = (string*)pkey; return v3_map_hash_bytes(s->str, s->len); }')
	g.writeln('static inline u64 v3_map_hash_int_1(void* pkey) { return v3_map_hash_bytes(pkey, 1); }')
	g.writeln('static inline u64 v3_map_hash_int_2(void* pkey) { return v3_map_hash_bytes(pkey, 2); }')
	g.writeln('static inline u64 v3_map_hash_int_4(void* pkey) { return v3_map_hash_bytes(pkey, 4); }')
	g.writeln('static inline u64 v3_map_hash_int_8(void* pkey) { return v3_map_hash_bytes(pkey, 8); }')
	g.writeln('static inline bool v3_map_eq_string(void* a, void* b) { string* sa = (string*)a; string* sb = (string*)b; return sa->len == sb->len && memcmp(sa->str, sb->str, sa->len) == 0; }')
	g.writeln('static inline bool v3_map_eq_int_1(void* a, void* b) { return memcmp(a, b, 1) == 0; }')
	g.writeln('static inline bool v3_map_eq_int_2(void* a, void* b) { return memcmp(a, b, 2) == 0; }')
	g.writeln('static inline bool v3_map_eq_int_4(void* a, void* b) { return memcmp(a, b, 4) == 0; }')
	g.writeln('static inline bool v3_map_eq_int_8(void* a, void* b) { return memcmp(a, b, 8) == 0; }')
	g.writeln('static inline void v3_map_clone_string(void* dest, void* pkey) { string cloned = string__clone(*(string*)pkey); memcpy(dest, &cloned, sizeof(string)); }')
	g.writeln('static inline void v3_map_clone_int_1(void* dest, void* pkey) { memcpy(dest, pkey, 1); }')
	g.writeln('static inline void v3_map_clone_int_2(void* dest, void* pkey) { memcpy(dest, pkey, 2); }')
	g.writeln('static inline void v3_map_clone_int_4(void* dest, void* pkey) { memcpy(dest, pkey, 4); }')
	g.writeln('static inline void v3_map_clone_int_8(void* dest, void* pkey) { memcpy(dest, pkey, 8); }')
	g.writeln('static inline void v3_map_free_string(void* pkey) { string__free((string*)pkey); }')
	g.writeln('static inline void v3_map_free_nop(void* pkey) { (void)pkey; }')
	g.writeln('static inline int array_index_int(Array a, int val) { for (int i = 0; i < a.len; i++) if (((int*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline bool array_contains_int(Array a, int val) { return array_index_int(a, val) >= 0; }')
	g.writeln('static inline int array_index_u8(Array a, u8 val) { for (int i = 0; i < a.len; i++) if (((u8*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline bool array_contains_u8(Array a, u8 val) { return array_index_u8(a, val) >= 0; }')
	g.writeln('static inline int array_index_string(Array a, string val) { string* data = (string*)a.data; for (int i = 0; i < a.len; i++) if (data[i].len == val.len && memcmp(data[i].str, val.str, val.len) == 0) return i; return -1; }')
	g.writeln('static inline bool array_contains_string(Array a, string val) { return array_index_string(a, val) >= 0; }')
	g.writeln('static inline bool array_eq_raw(Array a, Array b, int elem_size) { return a.len == b.len && (a.len == 0 || memcmp(a.data, b.data, (size_t)a.len * elem_size) == 0); }')
	g.writeln('static inline bool array_eq_string(Array a, Array b) { if (a.len != b.len) return false; string* ad = (string*)a.data; string* bd = (string*)b.data; for (int i = 0; i < a.len; i++) if (ad[i].len != bd[i].len || memcmp(ad[i].str, bd[i].str, ad[i].len) != 0) return false; return true; }')
	g.writeln('static inline bool fixed_array_contains_string(const string* a, int len, string val) { for (int i = 0; i < len; i++) if (a[i].len == val.len && memcmp(a[i].str, val.str, val.len) == 0) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_u8(const u8* a, int len, u8 val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_int(const int* a, int len, int val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline string Array_str(Array a) { if (a.element_size == 1) { u8* buf = (u8*)malloc((size_t)a.len + 1); if (a.len > 0) memcpy(buf, a.data, (size_t)a.len); buf[a.len] = 0; return (string){buf, a.len, 0}; } return (string){(u8*)"[]", 2, 1}; }')
	g.writeln('static inline string Array_string__join(Array a, string sep) {')
	g.writeln('\tif (a.len == 0) return (string){(u8*)"", 0, 1};')
	g.writeln('\tstring* data = (string*)a.data; int len = 0;')
	g.writeln('\tfor (int i = 0; i < a.len; i++) len += data[i].len + sep.len;')
	g.writeln('\tlen -= sep.len; u8* buf = (u8*)malloc(len + 1); int pos = 0;')
	g.writeln('\tfor (int i = 0; i < a.len; i++) { memcpy(buf + pos, data[i].str, data[i].len); pos += data[i].len; if (i != a.len - 1) { memcpy(buf + pos, sep.str, sep.len); pos += sep.len; } }')
	g.writeln('\tbuf[len] = 0; return (string){buf, len, 0};')
	g.writeln('}')
	g.writeln('#define array_string_join Array_string__join')
	g.writeln('#include <spawn.h>')
	g.writeln('extern char **environ;')
	g.writeln('static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {')
	g.writeln('\tint pipefd[2]; if (pipe(pipefd) != 0) return -1;')
	g.writeln('\tposix_spawn_file_actions_t fa; posix_spawn_file_actions_init(&fa);')
	g.writeln('\tposix_spawn_file_actions_adddup2(&fa, pipefd[1], 1);')
	g.writeln('\tposix_spawn_file_actions_adddup2(&fa, pipefd[1], 2);')
	g.writeln('\tposix_spawn_file_actions_addclose(&fa, pipefd[0]);')
	g.writeln('\tchar *argv[] = {"/bin/sh", "-c", (char*)cmd, NULL};')
	g.writeln('\tpid_t pid; int ret = posix_spawn(&pid, "/bin/sh", &fa, NULL, argv, environ);')
	g.writeln('\tposix_spawn_file_actions_destroy(&fa); close(pipefd[1]);')
	g.writeln('\tif (ret != 0) { close(pipefd[0]); return -1; }')
	g.writeln('\t*child_pid = pid; *read_fd = pipefd[0]; return 0;')
	g.writeln('}')
	g.writeln('static int v_os_exec_capture_start(char *const argv[], int *child_pid, int *read_fd) {')
	g.writeln('\tif (argv == NULL || argv[0] == NULL) return -1;')
	g.writeln('\tint pipefd[2]; if (pipe(pipefd) != 0) return -1;')
	g.writeln('\tposix_spawn_file_actions_t fa; posix_spawn_file_actions_init(&fa);')
	g.writeln('\tposix_spawn_file_actions_adddup2(&fa, pipefd[1], 1);')
	g.writeln('\tposix_spawn_file_actions_adddup2(&fa, pipefd[1], 2);')
	g.writeln('\tposix_spawn_file_actions_addclose(&fa, pipefd[0]);')
	g.writeln('\tpid_t pid; int ret = posix_spawnp(&pid, argv[0], &fa, NULL, argv, environ);')
	g.writeln('\tposix_spawn_file_actions_destroy(&fa); close(pipefd[1]);')
	g.writeln('\tif (ret != 0) { close(pipefd[0]); return -1; }')
	g.writeln('\t*child_pid = pid; *read_fd = pipefd[0]; return 0;')
	g.writeln('}')
	g.writeln('#ifndef max_int')
	g.writeln('#define max_int max_i32')
	g.writeln('#endif')
	g.writeln('#ifndef min_int')
	g.writeln('#define min_int min_i32')
	g.writeln('#endif')
	g.writeln('')
}

fn (g &FlatGen) needs_late_compat_decls() bool {
	for name in [
		'net.Addr',
		'http.Request',
		'orm.Table',
		'sqlite.Stmt',
		'fasthttp.Server',
		'mbedtls.SSLConn',
		'blowfish.Blowfish',
		'json2.Decoder',
		'pcre.Regex',
		'markdown.Markdown',
		'big.Integer',
		'time.Time',
	] {
		if name in g.struct_decl_infos {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) late_compat_decls() {
	if 'strconv.AtoF64Param' in g.struct_decl_infos {
		g.writeln('static inline Optional strconv__atof64(string s, strconv__AtoF64Param param) { (void)param; char buf[128]; int n = s.len < 127 ? s.len : 127; if (n < 0) n = 0; if (s.str != NULL && n > 0) memcpy(buf, s.str, (size_t)n); buf[n] = 0; return (Optional){.ok = true, .value = (int)strtod(buf, NULL)}; }')
	}
	if !g.needs_late_compat_decls() {
		return
	}
	g.writeln('static inline string v3_empty_string_lit(void) { return (string){.str = (u8*)"", .len = 0, .is_lit = 1}; }')
	g.writeln('static inline u8* v3_net_addr_arg_bytes(void* arg, size_t arg_size) { if (arg_size == sizeof(Array)) { return (u8*)((Array*)arg)->data; } return (u8*)arg; }')
	g.writeln('static inline net__Addr v3_net_new_ip_addr(u16 port, void* arg, size_t arg_size, int family, int len) { net__Addr a = (net__Addr){}; u8* bytes = v3_net_addr_arg_bytes(arg, arg_size); a.len = (u8)len; a.f = (u8)family; if (family == 2) { a.addr.Ip6.port = (u16)htons(port); if (bytes != NULL) memcpy(&a.addr.Ip6.addr[0], bytes, 16); } else { a.addr.Ip.port = (u16)htons(port); if (bytes != NULL) memcpy(&a.addr.Ip.addr[0], bytes, 4); } return a; }')
	g.writeln('static inline void net__set_addr_family(net__Addr* a, int family, u32 sockaddr_size) { if (a != NULL) { a->len = (u8)sockaddr_size; a->f = (u8)family; } }')
	g.writeln('#define net__new_ip6(port, addr) v3_net_new_ip_addr((port), (void*)&(addr), sizeof(addr), 2, sizeof(struct sockaddr_in6))')
	g.writeln('#define net__new_ip(port, addr) v3_net_new_ip_addr((port), (void*)&(addr), sizeof(addr), 1, sizeof(struct sockaddr_in))')
	g.writeln('static inline Optional_net__Addr net__temp_unix(void) { return (Optional_net__Addr){.ok = true, .value = (net__Addr){}}; }')
	g.writeln('static inline int net__Addr__family(net__Addr a) { return (int)a.f; }')
	g.writeln('static inline Optional net__Addr__port(net__Addr a) { (void)a; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline string net__Ip__str(net__Ip a) { (void)a; return v3_empty_string_lit(); }')
	g.writeln('static inline string net__Ip6__str(net__Ip6 a) { (void)a; return v3_empty_string_lit(); }')
	g.writeln('static inline u32 net__Addr__len(net__Addr* a) { if (a == NULL) return 0; return a->len != 0 ? a->len : (u32)sizeof(net__Addr); }')
	g.writeln('static inline Optional_Array v3_net_empty_addrs(void) { return (Optional_Array){.ok = true, .value = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(net__Addr)}}; }')
	g.writeln('static inline Optional_Array net__resolve_addrs(string addr, int family, int typ) { (void)addr; (void)family; (void)typ; return v3_net_empty_addrs(); }')
	g.writeln('static inline Optional_Array net__resolve_addrs_fuzzy(string addr, int typ) { (void)addr; (void)typ; return v3_net_empty_addrs(); }')
	g.writeln('static inline Optional net__wrap_getaddrinfo_error(int code) { (void)code; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_Array net__resolve_ipaddrs(string addr, int family, int typ) { (void)addr; (void)family; (void)typ; return v3_net_empty_addrs(); }')
	g.writeln('static inline string net__Addr__str(net__Addr a) { (void)a; return v3_empty_string_lit(); }')
	g.writeln('static inline net__Addr net__addr_from_socket_handle(int handle) { (void)handle; return (net__Addr){}; }')
	g.writeln('static inline Optional_net__Addr net__peer_addr_from_socket_handle(int handle) { (void)handle; return (Optional_net__Addr){.ok = true, .value = (net__Addr){}}; }')
	g.writeln('static inline Optional net__RawSocket__select(net__RawSocket* s, int test, i64 timeout) { (void)s; (void)test; (void)timeout; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional_string net__canonical_ipv6_from_bytes(Array b) { (void)b; return (Optional_string){.ok = true, .value = v3_empty_string_lit()}; }')
	g.writeln('static inline string net__format_ipv6_groups(array g) { (void)g; return v3_empty_string_lit(); }')
	g.writeln('static inline bool net__is_ipv4_mapped(array g) { (void)g; return false; }')
	g.writeln('static inline multi_return_int_int net__longest_zero_run(array g) { (void)g; return (multi_return_int_int){.arg0 = 0, .arg1 = 0}; }')
	g.writeln('static inline Optional_Array net__parse_ipv6_to_bytes(string s) { (void)s; return (Optional_Array){.ok = true, .value = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}}; }')
	g.writeln('static inline Optional net__parse_hex_group(string s) { (void)s; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional net__hex_digit(u8 c) { (void)c; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional_Array net__parse_dotted_quad(string s) { (void)s; return (Optional_Array){.ok = true, .value = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}}; }')
	g.writeln('static inline Optional_array net__parse_ipv6_multicast_addr(string multicast_addr) { (void)multicast_addr; return (Optional_array){.ok = true, .value = (array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}}; }')
	g.writeln('static inline Optional net__parse_ipv6_interface_index(string iface_addr) { (void)iface_addr; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional net__select_deadline(int handle, int test, time__Time deadline) { (void)handle; (void)test; (void)deadline; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional mbedtls__wait_for(int handle, int what, i64 timeout) { (void)handle; (void)what; (void)timeout; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_net__Addr net__UdpSocket__remote(net__UdpSocket* s) { (void)s; return (Optional_net__Addr){.ok = false}; }')
	g.writeln('static inline Optional net__UdpSocket__select(net__UdpSocket* s, int test, i64 timeout) { (void)s; (void)test; (void)timeout; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional net__TcpSocket__set_default_options(net__TcpSocket* s, int af) { (void)s; (void)af; return (Optional){.ok = true}; }')
	g.writeln("static inline bool u8__is_hex_digit(u8 c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }")
	g.writeln("static inline bool u8__is_alnum(u8 c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }")
	g.writeln("static inline int http__chunked_hex_value(u8 c) { if (c >= '0' && c <= '9') return c - '0'; if (c >= 'a' && c <= 'f') return c - 'a' + 10; if (c >= 'A' && c <= 'F') return c - 'A' + 10; return 0; }")
	g.writeln("static inline u8 http__header_lower_ascii_byte(u8 c) { return (c >= 'A' && c <= 'Z') ? (u8)(c + 32) : c; }")
	g.writeln('static inline bool http__header_key_eq(string a, string b) { if (a.len != b.len) return false; for (int i = 0; i < a.len; i++) { if (http__header_lower_ascii_byte(a.str[i]) != http__header_lower_ascii_byte(b.str[i])) return false; } return true; }')
	g.writeln('static inline Array arrays__uniq(Array a) { return a; }')
	g.writeln('static inline string string__bytestr(string s) { return s; }')
	g.writeln('typedef Optional (*_fn_ptr_67)(http__FetchConfig*);')
	g.writeln('static inline bool urllib__ishex(u8 c) { return u8__is_hex_digit(c); }')
	g.writeln("static inline u8 urllib__unhex(u8 c) { if (c >= '0' && c <= '9') return (u8)(c - '0'); if (c >= 'a' && c <= 'f') return (u8)(c - 'a' + 10); if (c >= 'A' && c <= 'F') return (u8)(c - 'A' + 10); return 0; }")
	g.writeln('static inline void sha256__block_generic(sha256__Digest* dig, Array p_) { (void)dig; (void)p_; }')
	g.writeln('static inline void sha256__Digest__free(sha256__Digest* d) { (void)d; }')
	g.writeln('static inline void sha256__Digest__init(sha256__Digest* d) { (void)d; }')
	g.writeln('static inline void sha256__Digest__reset(sha256__Digest* d) { (void)d; }')
	g.writeln('static inline sha256__Digest* sha256__Digest__clone(sha256__Digest* d) { (void)d; sha256__Digest* out = (sha256__Digest*)malloc(sizeof(sha256__Digest)); if (out != NULL) memset(out, 0, sizeof(sha256__Digest)); return out; }')
	g.writeln('static inline sha256__Digest* sha256__new(void) { sha256__Digest* out = (sha256__Digest*)malloc(sizeof(sha256__Digest)); if (out != NULL) memset(out, 0, sizeof(sha256__Digest)); return out; }')
	g.writeln('static inline sha256__Digest* sha256__new224(void) { return sha256__new(); }')
	g.writeln('static inline Optional sha256__Digest__write(sha256__Digest* d, Array p_) { (void)d; return (Optional){.ok = true, .value = p_.len}; }')
	g.writeln('static inline Array sha256__Digest__sum(sha256__Digest* d, Array b_in) { (void)d; return b_in; }')
	g.writeln('static inline Array sha256__Digest__checksum(sha256__Digest* d) { (void)d; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline Array sha256__sum(Array data) { (void)data; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline Array sha256__sum256(Array data) { return sha256__sum(data); }')
	g.writeln('static inline Array sha256__sum224(Array data) { return sha256__sum(data); }')
	g.writeln('static inline void sha256__block(sha256__Digest* dig, Array p) { (void)dig; (void)p; }')
	g.writeln('static inline int sha256__Digest__size(sha256__Digest* d) { (void)d; return 32; }')
	g.writeln('static inline int sha256__Digest__block_size(sha256__Digest* d) { (void)d; return 64; }')
	g.writeln('static inline string sha256__hexhash(string s) { (void)s; return v3_empty_string_lit(); }')
	g.writeln('static inline void sha1__block_generic(sha1__Digest* dig, Array p_) { (void)dig; (void)p_; }')
	g.writeln('static inline void sha1__Digest__free(sha1__Digest* d) { (void)d; }')
	g.writeln('static inline void sha1__Digest__init(sha1__Digest* d) { (void)d; }')
	g.writeln('static inline void sha1__Digest__reset(sha1__Digest* d) { (void)d; }')
	g.writeln('static inline sha1__Digest* sha1__Digest__clone(sha1__Digest* d) { (void)d; sha1__Digest* out = (sha1__Digest*)malloc(sizeof(sha1__Digest)); if (out != NULL) memset(out, 0, sizeof(sha1__Digest)); return out; }')
	g.writeln('static inline sha1__Digest* sha1__new(void) { sha1__Digest* out = (sha1__Digest*)malloc(sizeof(sha1__Digest)); if (out != NULL) memset(out, 0, sizeof(sha1__Digest)); return out; }')
	g.writeln('static inline Optional sha1__Digest__write(sha1__Digest* d, Array p_) { (void)d; return (Optional){.ok = true, .value = p_.len}; }')
	g.writeln('static inline Array sha1__Digest__sum(sha1__Digest* d, Array b_in) { (void)d; return b_in; }')
	g.writeln('static inline Array sha1__Digest__checksum(sha1__Digest* d) { (void)d; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline Array sha1__sum(Array data) { (void)data; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline void sha1__block(sha1__Digest* dig, Array p) { (void)dig; (void)p; }')
	g.writeln('static inline int sha1__Digest__size(sha1__Digest* d) { (void)d; return 20; }')
	g.writeln('static inline int sha1__Digest__block_size(sha1__Digest* d) { (void)d; return 64; }')
	g.writeln('static inline string sha1__hexhash(string s) { (void)s; return v3_empty_string_lit(); }')
	g.writeln('static inline Optional log__level_from_tag(string tag) { (void)tag; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional_string git__get_git_executable_path(void) { return (Optional_string){.ok = false}; }')
	g.writeln('static inline void base32__Encoding__encode_(base32__Encoding* enc, Array src_, Array* dst) { (void)enc; (void)src_; (void)dst; }')
	g.writeln('static inline Array binary__little_endian_get_u32(u32 v) { (void)v; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline Array binary__big_endian_get_u32(u32 v) { (void)v; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline Optional rand__int_u64(u64 max) { (void)max; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('typedef struct PGconn { int _dummy; } PGconn;')
	g.writeln('typedef struct PGresult { int _dummy; } PGresult;')
	g.writeln('typedef struct PGnotify { char* relname; int be_pid; char* extra; } PGnotify;')
	g.writeln('static inline PGconn* PQconnectdb(char* conninfo) { (void)conninfo; static PGconn conn; return &conn; }')
	g.writeln('static inline int PQstatus(PGconn* conn) { (void)conn; return 0; }')
	g.writeln('static inline int PQtransactionStatus(PGconn* conn) { (void)conn; return 0; }')
	g.writeln('static inline char* PQerrorMessage(PGconn* conn) { (void)conn; return ""; }')
	g.writeln('static inline PGresult* PQexec(PGconn* conn, char* query) { (void)conn; (void)query; static PGresult res; return &res; }')
	g.writeln('static inline int PQgetisnull(PGresult* res, int row, int col) { (void)res; (void)row; (void)col; return 1; }')
	g.writeln('static inline char* PQgetvalue(PGresult* res, int row, int col) { (void)res; (void)row; (void)col; return ""; }')
	g.writeln('static inline int PQresultStatus(PGresult* res) { (void)res; return 1; }')
	g.writeln('static inline int PQntuples(PGresult* res) { (void)res; return 0; }')
	g.writeln('static inline int PQnfields(PGresult* res) { (void)res; return 0; }')
	g.writeln('static inline char* PQfname(PGresult* res, int col) { (void)res; (void)col; return ""; }')
	g.writeln('static inline PGresult* PQexecParams(PGconn* conn, char* query, int nparams, void* types, void* vals, void* lens, void* formats, int result_format) { (void)conn; (void)query; (void)nparams; (void)types; (void)vals; (void)lens; (void)formats; (void)result_format; static PGresult res; return &res; }')
	g.writeln('static inline int PQputCopyData(PGconn* conn, void* buffer, int nbytes) { (void)conn; (void)buffer; (void)nbytes; return 1; }')
	g.writeln('static inline int PQputCopyEnd(PGconn* conn, char* errmsg) { (void)conn; (void)errmsg; return 1; }')
	g.writeln('static inline int PQgetCopyData(PGconn* conn, char** buffer, int async) { (void)conn; (void)buffer; (void)async; return -1; }')
	g.writeln('static inline PGresult* PQprepare(PGconn* conn, char* name, char* query, int nparams, void* param_types) { (void)conn; (void)name; (void)query; (void)nparams; (void)param_types; static PGresult res; return &res; }')
	g.writeln('static inline PGresult* PQexecPrepared(PGconn* conn, char* name, int nparams, void* vals, void* lens, void* formats, int result_format) { (void)conn; (void)name; (void)nparams; (void)vals; (void)lens; (void)formats; (void)result_format; static PGresult res; return &res; }')
	g.writeln('static inline void PQclear(PGresult* res) { (void)res; }')
	g.writeln('static inline void PQfreemem(void* ptr) { (void)ptr; }')
	g.writeln('static inline void PQfinish(PGconn* conn) { (void)conn; }')
	g.writeln('static inline PGnotify* PQnotifies(PGconn* conn) { (void)conn; return NULL; }')
	g.writeln('static inline int PQconsumeInput(PGconn* conn) { (void)conn; return 1; }')
	g.writeln('static inline int PQsocket(PGconn* conn) { (void)conn; return -1; }')
	g.writeln('static inline char* PQescapeLiteral(PGconn* conn, char* str, size_t len) { (void)conn; (void)len; return str; }')
	g.writeln('static inline void pg__pg_stmt_match_array(Array* types, Array* vals, Array* lens, Array* formats, Array data) { (void)types; (void)vals; (void)lens; (void)formats; (void)data; }')
	g.writeln('typedef void* _fn_ptr_64;')
	g.writeln('static inline Optional_string orm__orm_table_gen(int sql_dialect, orm__Table table, string q, bool defaults, int def_unique_len, Array fields, void* sql_from_v, bool unique) { (void)sql_dialect; (void)table; (void)q; (void)defaults; (void)def_unique_len; (void)fields; (void)sql_from_v; (void)unique; return (Optional_string){.ok = true, .value = v3_empty_string_lit()}; }')
	if 'sqlite.Stmt' !in g.struct_decl_infos {
		g.writeln('typedef struct sqlite__Stmt { void* stmt; void* db; } sqlite__Stmt;')
	}
	if 'sqlite.ConnectionPool' !in g.struct_decl_infos {
		g.writeln('typedef struct sqlite__ConnectionPool { int _dummy; } sqlite__ConnectionPool;')
	}
	g.writeln('static inline int sqlite__bind_array(sqlite__Stmt stmt, int** c, Array data) { (void)stmt; (void)c; (void)data; return 0; }')
	g.writeln('#ifndef orm__type_string')
	g.writeln('#define orm__type_string 9')
	g.writeln('#endif')
	g.writeln('#ifndef SQLITE_NULL')
	g.writeln('#define SQLITE_NULL 5')
	g.writeln('#endif')
	g.writeln('static inline Optional_string sqlite__sqlite_type_from_v(int typ) { (void)typ; return (Optional_string){.ok = true, .value = v3_empty_string_lit()}; }')
	g.writeln('static inline Optional_orm__Primitive sqlite__Stmt__sqlite_select_column(sqlite__Stmt stmt, int idx, int typ) { (void)stmt; (void)idx; (void)typ; return (Optional_orm__Primitive){.ok = false}; }')
	g.writeln('static inline int sqlite3_open(char* path, struct sqlite3** db) { (void)path; if (db != NULL) *db = NULL; return 0; }')
	g.writeln('static inline int sqlite3_open_v2(char* path, struct sqlite3** db, int flags, char* vfs) { (void)flags; (void)vfs; return sqlite3_open(path, db); }')
	g.writeln('static inline int sqlite3_close(struct sqlite3* db) { (void)db; return 0; }')
	g.writeln('static inline int sqlite3_busy_timeout(struct sqlite3* db, int ms) { (void)db; (void)ms; return 0; }')
	g.writeln('static inline i64 sqlite3_last_insert_rowid(struct sqlite3* db) { (void)db; return 0; }')
	g.writeln('static inline int sqlite3_changes(struct sqlite3* db) { (void)db; return 0; }')
	g.writeln('static inline int sqlite3_prepare_v2(struct sqlite3* db, char* query, int len, struct sqlite3_stmt** stmt, char** tail) { (void)db; (void)query; (void)len; (void)tail; if (stmt != NULL) *stmt = NULL; return 0; }')
	g.writeln('static inline int sqlite3_step(struct sqlite3_stmt* stmt) { (void)stmt; return 101; }')
	g.writeln('static inline int sqlite3_reset(struct sqlite3_stmt* stmt) { (void)stmt; return 0; }')
	g.writeln('static inline int sqlite3_finalize(struct sqlite3_stmt* stmt) { (void)stmt; return 0; }')
	g.writeln('static inline char* sqlite3_column_name(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return ""; }')
	g.writeln('static inline u8* sqlite3_column_text(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return (u8*)""; }')
	g.writeln('static inline int sqlite3_column_int(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return 0; }')
	g.writeln('static inline i64 sqlite3_column_int64(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return 0; }')
	g.writeln('static inline double sqlite3_column_double(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return 0.0; }')
	g.writeln('static inline int sqlite3_column_count(struct sqlite3_stmt* stmt) { (void)stmt; return 0; }')
	g.writeln('static inline int sqlite3_column_type(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return SQLITE_NULL; }')
	g.writeln('static inline int sqlite3_column_bytes(struct sqlite3_stmt* stmt, int col) { (void)stmt; (void)col; return 0; }')
	g.writeln('static inline char* sqlite3_errstr(int code) { (void)code; return ""; }')
	g.writeln('static inline char* sqlite3_errmsg(struct sqlite3* db) { (void)db; return ""; }')
	g.writeln('static inline void sqlite3_free(void* ptr) { (void)ptr; }')
	g.writeln('static inline int sqlite3_bind_null(struct sqlite3_stmt* stmt, int idx) { (void)stmt; (void)idx; return 0; }')
	g.writeln('static inline int sqlite3_bind_double(struct sqlite3_stmt* stmt, int idx, double val) { (void)stmt; (void)idx; (void)val; return 0; }')
	g.writeln('static inline int sqlite3_bind_int(struct sqlite3_stmt* stmt, int idx, int val) { (void)stmt; (void)idx; (void)val; return 0; }')
	g.writeln('static inline int sqlite3_bind_int64(struct sqlite3_stmt* stmt, int idx, i64 val) { (void)stmt; (void)idx; (void)val; return 0; }')
	g.writeln('static inline int sqlite3_bind_text(struct sqlite3_stmt* stmt, int idx, void* text, int len, void* dtor) { (void)stmt; (void)idx; (void)text; (void)len; (void)dtor; return 0; }')
	g.writeln('static inline i64 sqlite3_memory_used(void) { return 0; }')
	g.writeln('static inline struct sqlite3_vfs* sqlite3_vfs_find(char* name) { (void)name; return NULL; }')
	g.writeln('static inline int sqlite3_vfs_register(struct sqlite3_vfs* vfs, int make_default) { (void)vfs; (void)make_default; return 0; }')
	g.writeln('static inline int sqlite3_vfs_unregister(struct sqlite3_vfs* vfs) { (void)vfs; return 0; }')
	g.writeln('static inline void sqlite__ConnectionPool__close(sqlite__ConnectionPool* pool) { (void)pool; }')
	g.writeln("static inline bool regex__is_alnum(u8 in_char) { return ((in_char >= 'A' && in_char <= 'Z') || (in_char >= 'a' && in_char <= 'z') || (in_char >= '0' && in_char <= '9') || in_char == '_'); }")
	g.writeln("static inline i32 regex__RE__case_transform_char(regex__RE* re, i32 ch, bool upper) { (void)re; if (upper) { return (ch >= 'a' && ch <= 'z') ? ch - 32 : ch; } return (ch >= 'A' && ch <= 'Z') ? ch + 32 : ch; }")
	g.writeln('static inline multi_return_int_bool_bool_string_int regex__RE__parse_groups(regex__RE* re, string in_txt, int in_i) { (void)re; (void)in_txt; return (multi_return_int_bool_bool_string_int){0, true, false, v3_empty_string_lit(), in_i + 1}; }')
	g.writeln('static inline multi_return_int_int_u32 regex__RE__parse_bsls(regex__RE* re, string in_txt, int in_i) { (void)re; (void)in_txt; return (multi_return_int_int_u32){-1, in_i, (u32)0}; }')
	g.writeln('static inline string regex__RE__get_code(regex__RE* re) { (void)re; return v3_empty_string_lit(); }')
	g.writeln('static inline bool deflate__flush_stream_chunks(Array out, void* cb, void* userdata, deflate__InflateStreamState* state) { (void)out; (void)cb; (void)userdata; (void)state; return true; }')
	g.writeln('static inline Optional_deflate__InflateStreamResult deflate__inflate_with_callback(Array data, void* cb, void* userdata) { (void)cb; (void)userdata; return (Optional_deflate__InflateStreamResult){.ok = true, .value = (deflate__InflateStreamResult){.decoded = data}}; }')
	g.writeln('static inline Optional deflate__inflate_block_stream(deflate__BitReader* r, Array* out, deflate__HuffTree ll, deflate__HuffTree dist, void* cb, void* userdata, deflate__InflateStreamState* state) { (void)r; (void)out; (void)ll; (void)dist; (void)cb; (void)userdata; (void)state; return (Optional){.ok = true, .value = true}; }')
	g.writeln('static inline Optional_Array bcrypt__bcrypt(Array password, int cost, Array salt) { (void)cost; (void)salt; return (Optional_Array){.ok = true, .value = password}; }')
	g.writeln('static inline Array bcrypt__Hashed__hash_u8(bcrypt__Hashed* h) { (void)h; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline bool zstd__is_error(size_t code) { (void)code; return false; }')
	g.writeln('static inline string zstd__get_error_name(size_t code) { (void)code; return v3_empty_string_lit(); }')
	g.writeln('static inline Optional zstd__check_error(size_t code) { (void)code; return (Optional){.ok = true}; }')
	g.writeln('static inline int zstd__default_c_level(void) { return 0; }')
	g.writeln('static inline Optional_Array zstd__compress(Array data, zstd__CompressParams params) { (void)params; return (Optional_Array){.ok = true, .value = data}; }')
	g.writeln('static inline Optional_Array zstd__decompress(Array data, zstd__DecompressParams params) { (void)params; return (Optional_Array){.ok = true, .value = data}; }')
	g.writeln('static inline Optional_zstd__CCtxptr zstd__new_cctx(zstd__CompressParams params) { (void)params; return (Optional_zstd__CCtxptr){.ok = false}; }')
	g.writeln('static inline Optional zstd__CCtx__set(zstd__CCtx* c, int c_param, int val) { (void)c; (void)c_param; (void)val; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_size_t zstd__CCtx__compress_stream2(zstd__CCtx* c, zstd__OutBuffer* output, zstd__InBuffer* input, int mode) { (void)c; (void)output; (void)input; (void)mode; return (Optional_size_t){.ok = true, .value = 0}; }')
	g.writeln('static inline size_t zstd__CCtx__free_cctx(zstd__CCtx* c) { (void)c; return 0; }')
	g.writeln('static inline Optional_zstd__DCtxptr zstd__new_dctx(zstd__DecompressParams params) { (void)params; return (Optional_zstd__DCtxptr){.ok = false}; }')
	g.writeln('static inline Optional zstd__DCtx__set(zstd__DCtx* d, int d_param, int val) { (void)d; (void)d_param; (void)val; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_size_t zstd__DCtx__decompress_stream(zstd__DCtx* d, zstd__OutBuffer* output, zstd__InBuffer* input) { (void)d; (void)output; (void)input; return (Optional_size_t){.ok = true, .value = 0}; }')
	g.writeln('static inline size_t zstd__DCtx__free_dctx(zstd__DCtx* d) { (void)d; return 0; }')
	g.writeln('struct kevent { u64 ident; i16 filter; u16 flags; u32 fflags; ptrdiff_t data; void* udata; };')
	g.writeln('#ifndef EV_ADD')
	g.writeln('#define EV_ADD 0x0001')
	g.writeln('#endif')
	g.writeln('#ifndef EV_DELETE')
	g.writeln('#define EV_DELETE 0x0002')
	g.writeln('#endif')
	g.writeln('#ifndef EV_ENABLE')
	g.writeln('#define EV_ENABLE 0x0004')
	g.writeln('#endif')
	g.writeln('#ifndef EV_EOF')
	g.writeln('#define EV_EOF 0x8000')
	g.writeln('#endif')
	g.writeln('#ifndef EV_ERROR')
	g.writeln('#define EV_ERROR 0x4000')
	g.writeln('#endif')
	g.writeln('#ifndef EV_CLEAR')
	g.writeln('#define EV_CLEAR 0x0020')
	g.writeln('#endif')
	g.writeln('#ifndef EVFILT_READ')
	g.writeln('#define EVFILT_READ (-1)')
	g.writeln('#endif')
	g.writeln('#ifndef EVFILT_WRITE')
	g.writeln('#define EVFILT_WRITE (-2)')
	g.writeln('#endif')
	g.writeln('static inline int kqueue(void) { return -1; }')
	g.writeln('static inline int kevent(int kq, const struct kevent* changelist, int nchanges, struct kevent* eventlist, int nevents, const void* timeout) { (void)kq; (void)changelist; (void)nchanges; (void)eventlist; (void)nevents; (void)timeout; return 0; }')
	g.writeln('static inline stdatomic__AtomicVal* stdatomic__new_atomic(int val) { stdatomic__AtomicVal* out = (stdatomic__AtomicVal*)malloc(sizeof(stdatomic__AtomicVal)); if (out != NULL) out->val = val; return out; }')
	g.writeln('static inline int stdatomic__AtomicVal__load(stdatomic__AtomicVal a) { return a.val; }')
	g.writeln('static inline void stdatomic__AtomicVal__store(stdatomic__AtomicVal a, int val) { (void)a; (void)val; }')
	g.writeln('static inline int stdatomic__AtomicVal__add(stdatomic__AtomicVal a, int delta) { return a.val + delta; }')
	g.writeln('static inline int stdatomic__AtomicVal__sub(stdatomic__AtomicVal a, int delta) { return a.val - delta; }')
	g.writeln('static inline bool stdatomic__AtomicVal__compare_and_swap(stdatomic__AtomicVal a, int expected, int new_val) { (void)a; (void)expected; (void)new_val; return true; }')
	atomic_specs := g.generic_struct_specializations()
	if 'stdatomic.AtomicVal[bool]' in atomic_specs {
		g.writeln('static inline bool stdatomic__AtomicVal_bool__load(stdatomic__AtomicVal_bool a) { return a.val; }')
		g.writeln('static inline void stdatomic__AtomicVal_bool__store(stdatomic__AtomicVal_bool a, bool val) { (void)a; (void)val; }')
		g.writeln('static inline bool stdatomic__AtomicVal_bool__compare_and_swap(stdatomic__AtomicVal_bool a, bool expected, bool new_val) { (void)a; (void)expected; (void)new_val; return true; }')
	}
	if 'stdatomic.AtomicVal[int]' in atomic_specs {
		g.writeln('static inline int stdatomic__AtomicVal_int__load(stdatomic__AtomicVal_int a) { return a.val; }')
		g.writeln('static inline void stdatomic__AtomicVal_int__store(stdatomic__AtomicVal_int a, int val) { (void)a; (void)val; }')
		g.writeln('static inline int stdatomic__AtomicVal_int__add(stdatomic__AtomicVal_int a, int delta) { return a.val + delta; }')
		g.writeln('static inline int stdatomic__AtomicVal_int__sub(stdatomic__AtomicVal_int a, int delta) { return a.val - delta; }')
	}
	g.writeln('static inline multi_return_int_i64 fasthttp__send_file_bytes(i32 file_fd, i32 sock_fd, i64 offset, i64 nbytes) { (void)file_fd; (void)sock_fd; (void)offset; return (multi_return_int_i64){0, nbytes}; }')
	g.writeln('static inline void fasthttp__ev_set(struct kevent* ev, u64 ident, i16 filter, u16 flags, u32 fflags, ptrdiff_t data, void* udata) { if (ev != NULL) { ev->ident = ident; ev->filter = filter; ev->flags = flags; ev->fflags = fflags; ev->data = data; ev->udata = udata; } }')
	g.writeln('static inline Optional_fasthttp__Serverptr fasthttp__new_server(fasthttp__ServerConfig config) { fasthttp__Server* server = (fasthttp__Server*)malloc(sizeof(fasthttp__Server)); if (server != NULL) { memset(server, 0, sizeof(fasthttp__Server)); server->family = config.family; server->port = config.port; server->max_request_buffer_size = config.max_request_buffer_size; server->timeout_in_seconds = config.timeout_in_seconds; server->user_data = config.user_data; server->request_handler = config.handler; server->socket_fd = -1; server->poll_fd = -1; server->running = stdatomic__new_atomic(false); server->shutting_down = stdatomic__new_atomic(false); server->stopped = stdatomic__new_atomic(true); server->active_requests = stdatomic__new_atomic(0); } return (Optional_fasthttp__Serverptr){.ok = server != NULL, .value = server}; }')
	g.writeln('static inline int fasthttp__add_event(int kq, u64 ident, i16 filter, u16 flags, void* udata) { (void)kq; (void)ident; (void)filter; (void)flags; (void)udata; return 0; }')
	g.writeln('static inline void fasthttp__delete_event(int kq, u64 ident, i16 filter, void* udata) { (void)kq; (void)ident; (void)filter; (void)udata; }')
	g.writeln('static inline void fasthttp__close_conn(fasthttp__Server* server, int kq, void* c_ptr, map* clients) { (void)server; (void)kq; (void)c_ptr; (void)clients; }')
	g.writeln('static inline bool fasthttp__send_pending(void* c_ptr) { (void)c_ptr; return false; }')
	g.writeln('static inline void fasthttp__handle_write(fasthttp__Server* server, int kq, void* c_ptr, map* clients) { (void)server; (void)kq; (void)c_ptr; (void)clients; }')
	g.writeln('static inline void fasthttp__complete_response(fasthttp__Server* server, int kq, void* c_ptr, map* clients, bool remove_write_event) { (void)server; (void)kq; (void)c_ptr; (void)clients; (void)remove_write_event; }')
	g.writeln('static inline void fasthttp__process_request(fasthttp__Server* server, int kq, void* c_ptr, map* clients) { (void)server; (void)kq; (void)c_ptr; (void)clients; }')
	g.writeln('static inline void fasthttp__handle_read(fasthttp__Server* server, int kq, void* c_ptr, map* clients) { (void)server; (void)kq; (void)c_ptr; (void)clients; }')
	g.writeln('static inline void fasthttp__accept_clients(int kq, int listen_fd, map* clients) { (void)kq; (void)listen_fd; (void)clients; }')
	g.writeln('static inline void fasthttp__close_all_conns(fasthttp__Server* server, int kq, map* clients) { (void)server; (void)kq; (void)clients; }')
	g.writeln('static inline void fasthttp__Server__stop_accepting(fasthttp__Server* s) { (void)s; }')
	g.writeln('static inline Optional fasthttp__Server__run(fasthttp__Server* s) { (void)s; return (Optional){.ok = true}; }')
	g.writeln('static inline int fasthttp__chunked_hex_digit_value(u8 ch) { return http__chunked_hex_value(ch); }')
	g.writeln('static inline bool fasthttp__has_chunked_transfer_encoding_in_buf(u8* buf, int header_end) { (void)buf; (void)header_end; return false; }')
	g.writeln('static inline int fasthttp__parse_content_length_from_buf(u8* buf, int header_end) { (void)buf; (void)header_end; return 0; }')
	g.writeln('static inline Optional io__cp(io__Reader* src, io__Writer* dst, io__CopySettings params) { (void)src; (void)dst; (void)params; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional io__ReaderWriterImpl__read(io__ReaderWriterImpl* r, Array* buf) { (void)r; (void)buf; return (Optional){.ok = false}; }')
	g.writeln('static inline void io__BufferedWriter__shift_unwritten_to_front(io__BufferedWriter* b, int written) { (void)b; (void)written; }')
	g.writeln('static inline Optional io__BufferedWriter__write(io__BufferedWriter* b, Array src) { (void)b; return (Optional){.ok = true, .value = src.len}; }')
	g.writeln('static inline bool io__BufferedReader__fill_buffer(io__BufferedReader* r) { (void)r; return false; }')
	g.writeln('static inline string html__unescape_all(string input) { return input; }')
	g.writeln('#ifndef MBEDTLS_NET_PROTO_TCP')
	g.writeln('#define MBEDTLS_NET_PROTO_TCP 0')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_IS_CLIENT')
	g.writeln('#define MBEDTLS_SSL_IS_CLIENT 0')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_IS_SERVER')
	g.writeln('#define MBEDTLS_SSL_IS_SERVER 1')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_TRANSPORT_STREAM')
	g.writeln('#define MBEDTLS_SSL_TRANSPORT_STREAM 0')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_PRESET_DEFAULT')
	g.writeln('#define MBEDTLS_SSL_PRESET_DEFAULT 0')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_VERIFY_REQUIRED')
	g.writeln('#define MBEDTLS_SSL_VERIFY_REQUIRED 2')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_VERIFY_OPTIONAL')
	g.writeln('#define MBEDTLS_SSL_VERIFY_OPTIONAL 1')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_SSL_VERIFY_NONE')
	g.writeln('#define MBEDTLS_SSL_VERIFY_NONE 0')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_SSL_WANT_READ')
	g.writeln('#define MBEDTLS_ERR_SSL_WANT_READ (-0x6900)')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_SSL_WANT_WRITE')
	g.writeln('#define MBEDTLS_ERR_SSL_WANT_WRITE (-0x6880)')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_SSL_TIMEOUT')
	g.writeln('#define MBEDTLS_ERR_SSL_TIMEOUT (-0x6800)')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET')
	g.writeln('#define MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET (-0x6e00)')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY')
	g.writeln('#define MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY (-0x7880)')
	g.writeln('#endif')
	g.writeln('#ifndef MBEDTLS_ERR_NET_CONN_RESET')
	g.writeln('#define MBEDTLS_ERR_NET_CONN_RESET (-0x0050)')
	g.writeln('#endif')
	g.writeln('static inline void v_mbedtls_threading_setup(void) {}')
	g.writeln('static inline void mbedtls_net_init(void* ctx) { (void)ctx; }')
	g.writeln('static inline int mbedtls_net_connect(void* ctx, char* host, char* port, int proto) { (void)ctx; (void)host; (void)port; (void)proto; return -1; }')
	g.writeln('static inline int mbedtls_net_bind(void* ctx, char* host, char* port, int proto) { (void)ctx; (void)host; (void)port; (void)proto; return -1; }')
	g.writeln('static inline int mbedtls_net_accept(void* bind_ctx, void* client_ctx, void* client_ip, size_t buf_size, size_t* ip_len) { (void)bind_ctx; (void)client_ctx; (void)client_ip; (void)buf_size; if (ip_len != NULL) *ip_len = 0; return -1; }')
	g.writeln('static inline int mbedtls_net_recv(void* ctx, u8* buf, size_t len) { (void)ctx; (void)buf; (void)len; return -1; }')
	g.writeln('static inline int mbedtls_net_send(void* ctx, u8* buf, size_t len) { (void)ctx; (void)buf; return (int)len; }')
	g.writeln('static inline int mbedtls_net_recv_timeout(void* ctx, u8* buf, size_t len, u32 timeout) { (void)ctx; (void)buf; (void)len; (void)timeout; return -1; }')
	g.writeln('static inline void mbedtls_net_free(void* ctx) { (void)ctx; }')
	g.writeln('static inline void mbedtls_ssl_init(void* ctx) { (void)ctx; }')
	g.writeln('static inline int mbedtls_ssl_setup(void* ctx, void* conf) { (void)ctx; (void)conf; return 0; }')
	g.writeln('static inline void mbedtls_ssl_session_reset(void* ctx) { (void)ctx; }')
	g.writeln('static inline void mbedtls_ssl_conf_authmode(void* conf, int mode) { (void)conf; (void)mode; }')
	g.writeln('static inline void mbedtls_ssl_conf_rng(void* conf, int (*rng)(void*, u8*, size_t), void* p_rng) { (void)conf; (void)rng; (void)p_rng; }')
	g.writeln('static inline void mbedtls_ssl_set_bio(void* ssl, void* bio, void* send_cb, void* recv_cb, void* recv_timeout_cb) { (void)ssl; (void)bio; (void)send_cb; (void)recv_cb; (void)recv_timeout_cb; }')
	g.writeln('static inline int mbedtls_ssl_conf_own_cert(void* conf, void* cert, void* key) { (void)conf; (void)cert; (void)key; return 0; }')
	g.writeln('static inline void mbedtls_ssl_conf_ca_chain(void* conf, void* ca, void* crl) { (void)conf; (void)ca; (void)crl; }')
	g.writeln('static inline int mbedtls_ssl_set_hostname(void* ssl, char* hostname) { (void)ssl; (void)hostname; return 0; }')
	g.writeln('static inline int mbedtls_ssl_handshake(void* ssl) { (void)ssl; return 0; }')
	g.writeln('static inline int mbedtls_ssl_read(void* ssl, u8* buf, size_t len) { (void)ssl; (void)buf; (void)len; return -1; }')
	g.writeln('static inline int mbedtls_ssl_write(void* ssl, u8* buf, size_t len) { (void)ssl; (void)buf; return (int)len; }')
	g.writeln('static inline void mbedtls_ssl_free(void* ssl) { (void)ssl; }')
	g.writeln('static inline void mbedtls_ssl_config_init(void* conf) { (void)conf; }')
	g.writeln('static inline int mbedtls_ssl_config_defaults(void* conf, int endpoint, int transport, int preset) { (void)conf; (void)endpoint; (void)transport; (void)preset; return 0; }')
	g.writeln('static inline void mbedtls_ssl_config_free(void* conf) { (void)conf; }')
	g.writeln('static inline void mbedtls_ssl_conf_sni(void* conf, void* cb, void* data) { (void)conf; (void)cb; (void)data; }')
	g.writeln('static inline void mbedtls_ssl_set_hs_ca_chain(void* ssl, void* ca, void* crl) { (void)ssl; (void)ca; (void)crl; }')
	g.writeln('static inline int mbedtls_ssl_set_hs_own_cert(void* ssl, void* cert, void* key) { (void)ssl; (void)cert; (void)key; return 0; }')
	g.writeln('static inline void mbedtls_ssl_set_hs_authmode(void* ssl, int mode) { (void)ssl; (void)mode; }')
	g.writeln('static inline void mbedtls_pk_init(void* pk) { (void)pk; }')
	g.writeln('static inline void mbedtls_pk_free(void* pk) { (void)pk; }')
	g.writeln('static inline int mbedtls_pk_parse_key(void* pk, u8* key, size_t keylen, u8* pwd, size_t pwdlen, int (*rng)(void*, u8*, size_t), void* p_rng) { (void)pk; (void)key; (void)keylen; (void)pwd; (void)pwdlen; (void)rng; (void)p_rng; return 0; }')
	g.writeln('static inline int mbedtls_pk_parse_keyfile(void* pk, char* path, char* pwd, int (*rng)(void*, u8*, size_t), void* p_rng) { (void)pk; (void)path; (void)pwd; (void)rng; (void)p_rng; return 0; }')
	g.writeln('static inline void mbedtls_ctr_drbg_init(void* ctx) { (void)ctx; }')
	g.writeln('static inline int mbedtls_ctr_drbg_seed(void* ctx, int (*f)(void*, u8*, size_t), void* p_rng, u8* custom, size_t len) { (void)ctx; (void)f; (void)p_rng; (void)custom; (void)len; return 0; }')
	g.writeln('static inline void mbedtls_ctr_drbg_free(void* ctx) { (void)ctx; }')
	g.writeln('static inline int mbedtls_ctr_drbg_random(void* p_rng, u8* output, size_t len) { (void)p_rng; if (output != NULL) memset(output, 0, len); return 0; }')
	g.writeln('static inline void mbedtls_entropy_init(void* ctx) { (void)ctx; }')
	g.writeln('static inline void mbedtls_entropy_free(void* ctx) { (void)ctx; }')
	g.writeln('static inline int mbedtls_entropy_func(void* data, u8* output, size_t len) { (void)data; if (output != NULL) memset(output, 0, len); return 0; }')
	g.writeln('static inline void mbedtls_x509_crt_init(void* crt) { (void)crt; }')
	g.writeln('static inline void mbedtls_x509_crt_free(void* crt) { (void)crt; }')
	g.writeln('static inline int mbedtls_x509_crt_parse(void* crt, u8* buf, size_t len) { (void)crt; (void)buf; (void)len; return 0; }')
	g.writeln('static inline int mbedtls_x509_crt_parse_file(void* crt, char* path) { (void)crt; (void)path; return 0; }')
	g.writeln('static inline char* mbedtls_high_level_strerr(int code) { (void)code; return ""; }')
	g.writeln('static inline void mbedtls_debug_set_threshold(int level) { (void)level; }')
	g.writeln('static inline void mbedtls_ssl_conf_read_timeout(void* conf, u32 timeout) { (void)conf; (void)timeout; }')
	g.writeln('static inline int mbedtls_ssl_conf_alpn_protocols(void* conf, void* protos) { (void)conf; (void)protos; return 0; }')
	g.writeln('static inline void* mbedtls_ssl_get_alpn_protocol(void* ssl) { (void)ssl; return NULL; }')
	g.writeln('static inline void v_mbedtls_ssl_set_bio_nonblocking(void* ssl, void* bio) { (void)ssl; (void)bio; }')
	g.writeln("static inline u8 markdown__ascii_lower(u8 c) { return (c >= 'A' && c <= 'Z') ? (u8)(c + 32) : c; }")
	g.writeln('static inline string markdown__normalize_label(string s) { return s; }')
	g.writeln('static inline markdown__Node* markdown__BlockParser__parse_list_item(markdown__BlockParser* p, int indent) { (void)p; (void)indent; return NULL; }')
	g.writeln('static inline markdown__Markdown markdown__Markdown__new(markdown__Options opts) { return (markdown__Markdown){.opts = opts}; }')
	g.writeln('static inline void pcre__set_bitmap(array* bitmap, i32 r) { (void)bitmap; (void)r; }')
	g.writeln('static inline Optional_pcre__Regex pcre__compile(string pattern) { return (Optional_pcre__Regex){.ok = true, .value = (pcre__Regex){.pattern = pattern, .prog = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(pcre__Inst)}}}; }')
	g.writeln('static inline Optional_multi_return_Array_int_int pcre__parse_nodes(string pattern, int pos_start, i32 terminator, int group_counter_start, pcre__Flags passed_flags, map* group_map) { (void)pattern; (void)terminator; (void)passed_flags; (void)group_map; return (Optional_multi_return_Array_int_int){.ok = true, .value = (multi_return_Array_int_int){(Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(pcre__Node)}, pos_start, group_counter_start}}; }')
	g.writeln('static inline void pcre__Compiler__emit_class(pcre__Compiler* c, pcre__Node node) { (void)c; (void)node; }')
	g.writeln('static inline Optional_pcre__Match pcre__Regex__vm_match(pcre__Regex* r, string text, int start_pos, pcre__Machine* m) { (void)r; (void)text; (void)start_pos; (void)m; return (Optional_pcre__Match){.ok = false}; }')
	g.writeln("static inline u8 chunked__unhex(u8 c) { if (c >= '0' && c <= '9') return (u8)(c - '0'); if (c >= 'a' && c <= 'f') return (u8)(c - 'a' + 10); if (c >= 'A' && c <= 'F') return (u8)(c - 'A' + 10); return 0; }")
	g.writeln('static inline Optional_mbedtls__SSLConnptr ssl__new_ssl_conn(ssl__SSLConnectConfig config) { (void)config; return (Optional_mbedtls__SSLConnptr){.ok = false}; }')
	g.writeln('static inline Optional_net__Connection socks__handshake(net__Connection* con, string host, string username, string password) { (void)con; (void)host; (void)username; (void)password; return (Optional_net__Connection){.ok = false}; }')
	g.writeln('static inline big__MontgomeryContext big__Integer__montgomery(big__Integer m) { return (big__MontgomeryContext){.n = m}; }')
	g.writeln('static inline big__Integer big__integer_from_regular_string(string characters, u32 radix) { (void)characters; (void)radix; return (big__Integer){}; }')
	g.writeln('static inline big__Integer big__Integer__pow(big__Integer base, u32 exponent) { (void)exponent; return base; }')
	g.writeln('static inline big__Integer big__Integer__mod_pow(big__Integer base, u64 exponent, big__Integer modulus) { (void)exponent; (void)modulus; return base; }')
	g.writeln('static inline void big__Integer__dec(big__Integer* a) { (void)a; }')
	g.writeln('static inline void big__toom3_multiply_digit_array(Array operand_a, Array operand_b, Array* storage) { (void)operand_a; (void)operand_b; (void)storage; }')
	g.writeln('static inline void blowfish__expand_key(Array key, blowfish__Blowfish* bf) { (void)key; (void)bf; }')
	g.writeln('static inline void blowfish__expand_key_with_salt(Array key, Array salt, blowfish__Blowfish* bf) { (void)key; (void)salt; (void)bf; }')
	g.writeln('static inline multi_return_u32_u32 blowfish__setup_tables(u32 l, u32 r, blowfish__Blowfish* bf) { (void)bf; return (multi_return_u32_u32){l, r}; }')
	g.writeln('static inline void blowfish__Blowfish__encrypt(blowfish__Blowfish* bf, Array* dst, Array src) { (void)bf; (void)dst; (void)src; }')
	g.writeln('static inline Optional_blowfish__Blowfish blowfish__new_cipher(Array key) { (void)key; return (Optional_blowfish__Blowfish){.ok = true, .value = (blowfish__Blowfish){}}; }')
	g.writeln('static inline Optional_blowfish__Blowfish blowfish__new_salted_cipher(Array key, Array salt) { (void)key; (void)salt; return (Optional_blowfish__Blowfish){.ok = true, .value = (blowfish__Blowfish){}}; }')
	g.writeln('static inline void mbedtls__SSLListener__init_sni(mbedtls__SSLListener* l, void* get_cert_callback) { (void)l; (void)get_cert_callback; }')
	g.writeln('static inline orm__Primitive v3_orm_empty_primitive(void) { return (orm__Primitive){}; }')
	g.writeln('static inline orm__Primitive orm__bool_to_primitive(bool b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__f32_to_primitive(float b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__f64_to_primitive(double b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__i8_to_primitive(i8 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__i16_to_primitive(i16 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__int_to_primitive(int b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__int_literal_to_primitive(int b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__float_literal_to_primitive(double b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__i64_to_primitive(i64 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__u8_to_primitive(u8 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__u16_to_primitive(u16 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__u32_to_primitive(u32 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__u64_to_primitive(u64 b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__string_to_primitive(string b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline orm__Primitive orm__time_to_primitive(time__Time b) { (void)b; return v3_orm_empty_primitive(); }')
	g.writeln('static inline int orm__primitive_type(orm__Primitive value) { (void)value; return 0; }')
	g.writeln('static inline orm__Primitive orm__primitive_value(orm__Primitive value) { return value; }')
	g.writeln('static inline int orm__primitive_array_len(orm__Primitive value) { (void)value; return 0; }')
	g.writeln('static inline orm__AggregateValue orm__primitive_to_aggregate_value(orm__Primitive value) { return (orm__AggregateValue){.has_value = true, .value = value}; }')
	g.writeln('static inline int orm__tenant_filter_primitive_type(orm__Primitive value) { (void)value; return 0; }')
	g.writeln('static inline int orm__tenant_filter_array_primitive_type(Array value) { (void)value; return 0; }')
	g.writeln('static inline Optional orm__Tx__commit(orm__Tx* tx) { (void)tx; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional orm__Tx__rollback(orm__Tx* tx) { (void)tx; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_orm__Savepoint orm__Tx__savepoint(orm__Tx* tx) { (void)tx; return (Optional_orm__Savepoint){.ok = true, .value = (orm__Savepoint){}}; }')
	g.writeln('static inline Optional orm__Savepoint__rollback(orm__Savepoint* sp) { (void)sp; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_Array orm__Tx__select(orm__Tx* tx, orm__SelectConfig config, orm__QueryData data, orm__QueryData where) { (void)tx; (void)config; (void)data; (void)where; return (Optional_Array){.ok = true, .value = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(Array)}}; }')
	g.writeln('static inline Optional orm__DB__orm_savepoint(orm__DB* db, string name) { (void)db; (void)name; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional orm__DB__orm_rollback_to(orm__DB* db, string name) { (void)db; (void)name; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional orm__DB__orm_release_savepoint(orm__DB* db, string name) { (void)db; (void)name; return (Optional){.ok = true}; }')
	g.writeln('static inline json2__Token json2__Scanner__text_scan(json2__Scanner* s) { (void)s; return (json2__Token){}; }')
	g.writeln('static inline json2__Token json2__Scanner__num_scan(json2__Scanner* s) { (void)s; return (json2__Token){}; }')
	g.writeln('static inline Optional_json2__Token json2__ReaderScanner__text_scan(json2__ReaderScanner* s, int line, int col) { (void)s; (void)line; (void)col; return (Optional_json2__Token){.ok = false}; }')
	g.writeln('static inline Optional_json2__Token json2__ReaderScanner__num_scan(json2__ReaderScanner* s, int line, int col) { (void)s; (void)line; (void)col; return (Optional_json2__Token){.ok = false}; }')
	g.writeln('static json2__ValueInfo v3_json2_value_info_zero;')
	g.writeln('#define json2__LinkedList__push(list, value) ((void)0)')
	g.writeln('#define json2__LinkedList__last(list) (&v3_json2_value_info_zero)')
	g.writeln('static inline string json2__Any__json_str(json2__Any f) { (void)f; return v3_empty_string_lit(); }')
	g.writeln('static inline Array json2__Any__as_array(json2__Any f) { (void)f; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(json2__Any)}; }')
	g.writeln('static inline Array json2__Any__arr(json2__Any f) { return json2__Any__as_array(f); }')
	g.writeln('static inline string json2__map_stringAny__str(map f) { (void)f; return v3_empty_string_lit(); }')
	g.writeln('static inline string json2__Array_Any__str(Array f) { (void)f; return v3_empty_string_lit(); }')
	g.writeln('static inline Optional json2__Decoder__increment(json2__Decoder* checker, string message) { (void)checker; (void)message; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__skip_whitespace(json2__Decoder* checker, string message) { (void)checker; (void)message; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_json_format(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_string(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_number(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_boolean(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_null(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_array(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__check_object(json2__Decoder* checker) { (void)checker; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional json2__Decoder__checker_error(json2__Decoder* checker, string message) { (void)checker; (void)message; return (Optional){.ok = false}; }')
	g.writeln('static inline bool json2__Decoder__sumtype_type_field_matches(json2__Decoder* decoder, json2__Node* type_field_node, string expected) { (void)decoder; (void)type_field_node; (void)expected; return false; }')
	g.writeln('static inline Optional_string json2__scientific_number_to_integer_string(string str) { return (Optional_string){.ok = true, .value = str}; }')
	g.writeln('static inline string log__tag_to_file(int level, bool short_tag) { (void)level; (void)short_tag; return v3_empty_string_lit(); }')
	g.writeln('static inline string log__tag_to_console(int level, bool short_tag) { (void)level; (void)short_tag; return v3_empty_string_lit(); }')
	g.writeln('static inline void log__free_logger(log__Logger* logger) { (void)logger; }')
	g.writeln('static inline bool time__Time__eq(time__Time a, time__Time b) { return a.unix == b.unix && a.year == b.year && a.month == b.month && a.day == b.day && a.hour == b.hour && a.minute == b.minute && a.second == b.second && a.nanosecond == b.nanosecond && a.is_local == b.is_local; }')
	g.writeln('static inline string time__Time__format_rfc3339(time__Time t) { (void)t; return v3_empty_string_lit(); }')
	g.writeln('static inline string time__Time__format_rfc3339_micro(time__Time t) { (void)t; return v3_empty_string_lit(); }')
	g.writeln('static inline string time__Time__format_rfc3339_nano(time__Time t) { (void)t; return v3_empty_string_lit(); }')
	g.writeln('static inline Optional_http__Response http__get(string url) { (void)url; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__post(string url, string data) { (void)url; (void)data; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__post_json(string url, string data) { (void)url; (void)data; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__post_form(string url, map data) { (void)url; (void)data; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__post_form_with_cookies(string url, map data, map cookies) { (void)url; (void)data; (void)cookies; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__post_multipart_form(string url, http__PostMultipartFormConfig conf) { (void)url; (void)conf; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__patch(string url, string data) { (void)url; (void)data; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__head(string url) { (void)url; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__delete(string url) { (void)url; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__Request http__prepare(http__FetchConfig config) { (void)config; return (Optional_http__Request){.ok = false}; }')
	g.writeln('static inline Optional_http__Response http__fetch(http__FetchConfig config) { (void)config; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline Optional_http__H2ClientResponse http__H2Conn__read_response(http__H2Conn* c, u32 stream_id, http__H2ClientRequest req) { (void)c; (void)stream_id; (void)req; return (Optional_http__H2ClientResponse){.ok = false}; }')
	g.writeln('static inline Optional http__H2Conn__fill_at_least(http__H2Conn* c, int n) { (void)c; (void)n; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional http__H2Conn__write_all(http__H2Conn* c, Array data) { (void)c; (void)data; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional http__H2ServerConn__fill_at_least(http__H2ServerConn* c, int n) { (void)c; (void)n; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional http__H2ServerConn__write_all(http__H2ServerConn* c, Array data) { (void)c; (void)data; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_http__Response http__Request__h2_exchange(http__Request* req, http__H2Conn* conn, int method, string host_name, int port, string path, string data, http__Header header) { (void)req; (void)conn; (void)method; (void)host_name; (void)port; (void)path; (void)data; (void)header; return (Optional_http__Response){.ok = false}; }')
	g.writeln('static inline void http__h2_hpack_write_int(Array* out, u64 value, int prefix_bits, u8 high_bits) { (void)out; (void)value; (void)prefix_bits; (void)high_bits; }')
	g.writeln('static inline void http__h2_encode_string(Array* out, string s) { (void)out; (void)s; }')
	g.writeln('static inline bool http__h2_is_sensitive(string name) { (void)name; return false; }')
	g.writeln('static inline multi_return_int_bool http__h2_hpack_find_static(string name, string value) { (void)name; (void)value; return (multi_return_int_bool){.arg0 = 0, .arg1 = false}; }')
	g.writeln('static inline Array http__H2HpackEncoder__encode(http__H2HpackEncoder* e, Array fields) { (void)e; (void)fields; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(u8)}; }')
	g.writeln('static inline void http__H2HpackEncoder__encode_field(http__H2HpackEncoder* e, Array* out, http__H2HeaderField f) { (void)e; (void)out; (void)f; }')
	g.writeln('static inline Optional_http__H2HeaderField http__H2HpackDecoder__lookup(http__H2HpackDecoder* d, u64 idx) { (void)d; (void)idx; return (Optional_http__H2HeaderField){.ok = false}; }')
	g.writeln('static inline Optional_http__H2HeaderField http__H2HpackDecoder__read_literal(http__H2HpackDecoder* d, http__H2HpackReader* r, int prefix_bits) { (void)d; (void)r; (void)prefix_bits; return (Optional_http__H2HeaderField){.ok = false}; }')
	g.writeln('static inline Optional_Array http__H2HpackDecoder__decode(http__H2HpackDecoder* d, Array block) { (void)d; (void)block; return (Optional_Array){.ok = true, .value = (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(http__H2HeaderField)}}; }')
	g.writeln('static inline huffman__Table http__build_h2_huffman_table(void) { return (huffman__Table){}; }')
	g.writeln('static inline Array http__h2_huffman_encode(Array input) { return input; }')
	g.writeln('static inline Optional_Array http__h2_huffman_decode(Array input) { return (Optional_Array){.ok = true, .value = input}; }')
	g.writeln('static inline Optional http__parse_chunked_size_line(Array line) { (void)line; return (Optional){.ok = true, .value = 0}; }')
	g.writeln('static inline Optional_http__ReceivedResponseInfo http__Request__receive_all_data_from_cb_in_builder(http__Request* req, Array* content, void* con, void* receive_chunk_cb) { (void)req; (void)content; (void)con; (void)receive_chunk_cb; return (Optional_http__ReceivedResponseInfo){.ok = true, .value = (http__ReceivedResponseInfo){}}; }')
	g.writeln('static inline Optional_http__Request http__parse_request(io__BufferedReader* reader) { (void)reader; return (Optional_http__Request){.ok = false}; }')
	g.writeln('static inline void* http__new_handler_worker(int wid, chan ch, http__Handler handler, int max_keep_alive_requests) { (void)wid; (void)ch; (void)handler; (void)max_keep_alive_requests; return NULL; }')
	g.writeln('static inline Optional_string http__read_proxy_connect_response(net__TcpConn* tcp) { (void)tcp; return (Optional_string){.ok = false}; }')
	g.writeln('static inline void* http__new_tls_handler_worker(int wid, chan ch, http__Handler handler, int max_keep_alive_requests, http__TlsIdleConnTracker* idle_conns) { (void)wid; (void)ch; (void)handler; (void)max_keep_alive_requests; (void)idle_conns; return NULL; }')
	g.writeln('static inline Array http__Header__keys(http__Header h) { (void)h; return (Array){.data = NULL, .offset = 0, .len = 0, .cap = 0, .flags = 0, .element_size = sizeof(string)}; }')
	g.writeln('#ifndef http__CommonHeader__str')
	g.writeln('static inline string http__CommonHeader__str(int h) { (void)h; return v3_empty_string_lit(); }')
	g.writeln('#endif')
	g.writeln("static inline bool veb__ascii_eq_ignore_case(string a, string b) { if (a.len != b.len) return false; for (int i = 0; i < a.len; i++) { u8 ca = a.str[i]; u8 cb = b.str[i]; if (ca >= 'A' && ca <= 'Z') ca += 32; if (cb >= 'A' && cb <= 'Z') cb += 32; if (ca != cb) return false; } return true; }")
	g.writeln('static inline string veb__Context__ip(veb__Context* ctx) { (void)ctx; return v3_empty_string_lit(); }')
	g.writeln('static inline bool veb__send_compressed_response(veb__Context* ctx, int encoding) { (void)ctx; (void)encoding; return true; }')
	g.writeln('static inline Optional_fasthttp__HttpResponse veb__content_length_validation_response(fasthttp__HttpRequest req, http__Request parsed) { (void)req; (void)parsed; return (Optional_fasthttp__HttpResponse){.ok = false}; }')
	g.writeln('static inline Optional veb__read_exact_bytes(io__BufferedReader* reader, Array* buf) { (void)reader; (void)buf; return (Optional){.ok = true}; }')
	g.writeln('static inline Optional_veb__Result veb__Context__serve_compressed_static(veb__Context* ctx, string content_type, string file_path, string data, int encoding) { (void)ctx; (void)content_type; (void)file_path; (void)data; (void)encoding; return (Optional_veb__Result){.ok = false}; }')
	g.writeln('static inline highlight__Lang highlight__init_d(void) { return (highlight__Lang){}; }')
	g.writeln('static inline highlight__Lang highlight__init_v(void) { return (highlight__Lang){}; }')
	g.writeln('static inline Optional_time__Time time__parse_iso8601(string s) { (void)s; return (Optional_time__Time){.ok = true, .value = (time__Time){}}; }')
	g.writeln('static inline Optional_time__Time time__parse(string s) { (void)s; return (Optional_time__Time){.ok = true, .value = (time__Time){}}; }')
	g.writeln('static inline Optional_time__Time time__DateTimeParser__parse(time__DateTimeParser* p) { (void)p; return (Optional_time__Time){.ok = true, .value = (time__Time){}}; }')
	g.writeln('static inline string time__Time__custom_format(time__Time t, string s) { (void)t; (void)s; return v3_empty_string_lit(); }')
	g.writeln('static inline void time__Time__push_to_http_header(time__Time t, Array* buffer) { (void)t; (void)buffer; }')
	g.writeln('static inline bool validation__is_repository_name_valid(string value) { (void)value; return true; }')
	g.writeln('static inline bool validation__is_username_valid(string value) { (void)value; return true; }')
	g.writeln('')
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
			c_elem := g.tc.c_type(typ.elem_type)
			len_expr := g.fixed_array_len_value(typ)
			init := if g.has_zero_sized_leading_init_slot(typ) { '' } else { ' = {0}' }
			g.writeln('${c_elem} ${c_name(name)}[${len_expr}]${init};')
			continue
		}
		ct := g.tc.c_type(typ)
		if ct == 'void' {
			continue
		}
		if typ is types.Struct && typ.name.starts_with('C.') {
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
// Only initializers that translate to a plain `name = expr;` assignment are
// emitted. Fixed-array globals (`[N]T{}`) are skipped: C zero-initializes them
// already and a C array is not assignable. `&Struct{}` is emitted as a
// self-contained heap allocation (`(T*)memdup(&(T){...}, sizeof(T))`), so it is
// safe. Other prefix/array initializers that would need a dropped temporary are
// skipped, leaving the global zero/NULL — no regression versus never
// initializing globals at all.
fn (mut g FlatGen) emit_global_inits() {
	old_module := g.tc.cur_module
	for qname in g.global_init_order {
		val_id := g.global_inits[qname] or { continue }
		if int(val_id) < 0 {
			continue
		}
		if typ := g.global_types[qname] {
			if typ is types.ArrayFixed {
				continue
			}
		}
		if !g.is_safe_global_init(val_id) {
			continue
		}
		if mod := g.global_modules[qname] {
			g.tc.cur_module = mod
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
		g.runtime_inits << '\t${target} = ${expr_str};'
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
	expr_str := if g.is_const_expr(val_id) {
		g.const_expr_to_string(val_id, []string{})
	} else {
		g.expr_to_string(val_id)
	}
	if expr_str.trim_space().len == 0 {
		g.tc.cur_module = old_module
		return
	}
	v_type := g.tc.resolve_type(val_id)
	ct := g.tc.c_type(v_type)
	qname := g.const_ident_c_name(name)
	mut is_static_const := g.is_const_expr(val_id) && !g.const_expr_needs_runtime_storage(expr_str)
	if v_type is types.ArrayFixed && v_type.elem_type is types.ArrayFixed {
		is_static_const = false
	}
	if !is_static_const {
		if v_type is types.ArrayFixed {
			c_elem := g.tc.c_type(v_type.elem_type)
			len_expr := g.fixed_array_len_value(v_type)
			g.writeln('${c_elem} ${qname}[${len_expr}];')
		} else if ct != 'void' {
			g.writeln('${ct} ${qname};')
		}
		g.tc.cur_module = old_module
		return
	}
	if v_type is types.String {
		g.writeln('string ${qname} = ${expr_str};')
	} else if v_type is types.ArrayFixed {
		c_elem := g.tc.c_type(v_type.elem_type)
		g.writeln('const ${c_elem} ${qname}[] = ${expr_str};')
	} else if v_type is types.Primitive || v_type is types.Char || v_type is types.Rune
		|| v_type is types.ISize || v_type is types.USize || v_type is types.Enum
		|| ct in ['bool', 'char', 'i8', 'i16', 'i32', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'float', 'double', 'isize', 'usize'] {
		if qname == 'max_len' && ct == 'int' {
			g.writeln('enum { ${qname} = ${expr_str} };')
		} else {
			g.writeln('#define ${qname} (${expr_str})')
		}
	} else {
		g.writeln('const ${ct} ${qname} = ${expr_str};')
	}
	g.tc.cur_module = old_module
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
		g.runtime_inits << '\tmap__set(&${target}, &(${c_key}[]){${key}}, &(${c_val}[]){${val}});'
	}
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
	g.sb = old_sb
	g.line_start = old_line_start
	return result
}

fn (g &FlatGen) is_const_expr(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .enum_val,
		.sizeof_expr {
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
