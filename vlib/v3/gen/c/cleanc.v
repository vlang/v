module c

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
	interfaces              map[string][]string
	const_vals              map[string]flat.NodeId
	const_modules           map[string]string
	global_modules          map[string]string
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
	cur_fn_ret              types.Type = types.Type(types.void_)
	cur_fn_ret_is_optional  bool
	cur_fn_ret_base         types.Type = types.Type(types.void_)
	expected_expr_type      types.Type = types.Type(types.void_)
	expected_enum           string
	needed_optional_types   map[string]string
	emitted_fns             map[string]bool
	array_method_cache      map[string]string
}

pub fn FlatGen.new() FlatGen {
	return FlatGen{
		sb:            strings.new_builder(4096)
		str_lits:      []string{}
		defers:        []flat.NodeId{}
		runtime_inits: []string{}
		line_start:    true
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
	g.used_fn_names = []string{}
	for name, is_used in used_fns {
		if is_used {
			g.used_fn_names << name
		}
	}
	g.tc = unsafe { tc }
	if g.tc.a == unsafe { nil } {
		g.tc.collect(a)
	}
	g.has_builtins = g.tc.has_builtins
	g.collect_gen_info()
	g.preseed_struct_fn_ptr_types()
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
	g.fn_ptr_typedefs()
	g.struct_decls()
	g.builtin_compat_decls()
	g.optional_typedefs()
	g.multi_return_typedefs()
	g.global_decls()
	g.forward_decls()
	g.register_interface_strings()
	g.string_literals()
	g.interface_method_stubs()
	g.sb.write_string(const_code)
	if g.runtime_inits.len > 0 {
		g.writeln('void _vinit() {')
		for ri in g.runtime_inits {
			g.writeln(ri)
		}
		g.writeln('}')
		g.writeln('')
	}
	g.sb.write_string(fn_code)
	result := g.sb.str()
	return result
}

fn (mut g FlatGen) collect_gen_info() {
	mut cur_module := ''
	for node_idx in 0 .. g.a.nodes.len {
		node := g.a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
				g.tc.cur_module = cur_module
			}
			.module_decl {
				cur_module = node.value
				g.tc.cur_module = cur_module
			}
			.fn_decl {
				full_name := qualify_name_in_module(cur_module, node.value)
				mut ptypes := []types.Type{}
				g.tc.cur_module = cur_module
				for i in 0 .. node.children_count {
					child := g.a.child_node(&node, i)
					if child.kind == .param {
						raw_pt := g.tc.parse_type(child.typ)
						pt := raw_pt
						ptypes << raw_pt
						if pt is types.FnType {
							g.resolve_fn_ptr_type(g.tc.c_type(raw_pt))
						}
					}
				}
				g.register_fn_decl_param_types(node.value, full_name, ptypes)
			}
			.struct_decl {
				full_name := qualify_name_in_module(cur_module, node.value)
				g.register_struct_decl_info(node.value, full_name, cur_module, node)
			}
			.global_decl {
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
					g.tc.file_scope.insert(f.value, ft)
					if qname != f.value {
						g.tc.file_scope.insert(qname, ft)
					}
				}
			}
			.enum_decl {
				is_flag := node.typ == 'flag'
				mut val := 0
				enum_name := qualify_name_in_module(cur_module, node.value)
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.children_count > 0 {
						ev := g.a.child_node(f, 0)
						if ev.kind == .int_literal {
							val = ev.value.int()
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
			}
			.interface_decl {
				mut methods := []string{}
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.kind == .interface_field && f.op == .dot {
						methods << f.value
					}
				}
				g.interfaces[qualify_name_in_module(cur_module, node.value)] = methods
			}
			.const_decl {
				for i in 0 .. node.children_count {
					f := g.a.child_node(&node, i)
					if f.kind == .const_field && f.children_count > 0 {
						g.const_vals[f.value] = g.a.child(f, 0)
						g.const_modules[f.value] = cur_module
					}
				}
			}
			.import_decl {
				g.modules[node.typ] = node.value
			}
			else {}
		}
	}
	g.modules['strings'] = 'strings'
}

fn (mut g FlatGen) register_fn_decl_param_types(name string, full_name string, ptypes []types.Type) {
	if name !in g.fn_decl_param_types {
		g.fn_decl_param_types[name] = ptypes.clone()
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

fn (mut g FlatGen) gen_expr_with_expected_type(id flat.NodeId, expected types.Type) {
	old_expected := g.expected_expr_type
	g.expected_expr_type = expected
	actual := g.usable_expr_type(id)
	node := g.a.nodes[int(id)]
	if expected is types.Array && node.kind == .array_literal {
		elem_type := if node.children_count > 0 {
			g.tc.resolve_type(g.a.child(&node, 0))
		} else {
			expected.elem_type
		}
		g.gen_array_literal_value(node, elem_type)
		g.expected_expr_type = old_expected
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
		return
	}
	g.gen_expr(id)
	g.expected_expr_type = old_expected
}

fn (mut g FlatGen) optional_none_type(id flat.NodeId) types.Type {
	if typ := g.tc.expr_type(id) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	if g.expected_expr_type is types.OptionType || g.expected_expr_type is types.ResultType {
		return g.expected_expr_type
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

fn (g &FlatGen) const_ref_name(name string) string {
	if name in g.const_vals {
		return name
	}
	if name.contains('.') {
		cname := c_name(name)
		if cname in g.const_vals {
			return cname
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
	mod := if short_name in g.const_modules { g.const_modules[short_name] } else { '' }
	if mod.len == 0 {
		return short_name
	}
	ref_mod := name.all_before_last(sep)
	if ref_mod == mod || ref_mod == mod.all_after_last('.') {
		return short_name
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
			if target_type !is types.Primitive && target_type !is types.Char
				&& target_type !is types.Rune && target_type !is types.ISize
				&& target_type !is types.USize && target_type !is types.Pointer
				&& target_type !is types.Enum {
				return g.expr_to_string(id)
			}
			ct := g.tc.c_type(target_type)
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
		.int_literal, .float_literal, .bool_literal, .char_literal, .enum_val, .sizeof_expr {
			g.expr_to_string(id)
		}
		else {
			g.expr_to_string(id)
		}
	}
}

fn (g &FlatGen) const_ident_c_name(name string) string {
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

fn (mut g FlatGen) fixed_array_len_raw(raw_len string, fallback int) string {
	if raw_len.len == 0 {
		return '${fallback}'
	}
	clean_len := raw_len.replace('_', '')
	if clean_len.len > 0 && clean_len[0] >= `0` && clean_len[0] <= `9` {
		return clean_len
	}
	if raw_len in g.const_vals {
		expr := g.const_expr_to_string(g.const_vals[raw_len], []string{})
		if expr.trim_space().len > 0 {
			return expr
		}
		return g.const_ident_c_name(raw_len)
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
			g.write(node.value)
		}
		.bool_literal {
			g.write(node.value)
		}
		.char_literal {
			v := node.value
			if v.starts_with('c:') {
				cv := v[2..]
				g.write('"${cv}"')
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
			panic('internal error: string interpolation reached C backend after transform')
		}
		.ident {
			looked_up := g.tc.cur_scope.lookup(node.value) or { types.Type(types.void_) }
			is_local := looked_up !is types.Void
			if !is_local && node.value in g.const_vals {
				mod := if node.value in g.const_modules { g.const_modules[node.value] } else { '' }
				if mod.len > 0 && mod != 'main' && mod != 'builtin' {
					g.write(c_name('${mod}.${node.value}'))
				} else {
					g.write(c_name(node.value))
				}
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
				g.write('${g.enum_vals[node.value]}')
				return
			}
			if g.expected_enum.len > 0 {
				ekey := '${g.expected_enum}.${node.value}'
				if ekey in g.enum_vals {
					g.write('${g.enum_vals[ekey]}')
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
			g.gen_call(id, node)
		}
		.infix {
			lhs_id := g.a.child(&node, 0)
			rhs_id := g.a.child(&node, 1)
			lhs_type := g.tc.resolve_type(lhs_id)
			rhs_type := g.tc.resolve_type(rhs_id)
			if lhs_type is types.String || rhs_type is types.String {
				if g.gen_string_infix_fallback(node, lhs_id, rhs_id) {
					return
				}
			}
			if lhs_type is types.Enum {
				g.expected_enum = lhs_type.name
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
				g.gen_expr(rhs_id)
			} else {
				lhs_node := g.a.nodes[int(lhs_id)]
				rhs_node := g.a.nodes[int(rhs_id)]
				if lhs_node.kind == .infix {
					g.write('(')
					g.gen_expr(lhs_id)
					g.write(')')
				} else {
					g.gen_expr(lhs_id)
				}
				g.write(' ${g.op_str(node.op)} ')
				if rhs_node.kind == .infix {
					g.write('(')
					g.gen_expr(rhs_id)
					g.write(')')
				} else {
					g.gen_expr(rhs_id)
				}
			}
			g.expected_enum = ''
		}
		.prefix {
			child_id := g.a.child(&node, 0)
			child := g.a.nodes[int(child_id)]
			if node.op == .amp && child.kind == .struct_init {
				g.gen_heap_struct_init(child)
			} else if node.op == .amp && child.kind == .cast_expr {
				target_type := g.tc.parse_type(child.value)
				ct := g.tc.c_type(target_type)
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
				panic('internal error: non-map membership reached C backend after transform: rhs=${rhs_type.name()} kind=${rhs.kind} value=${rhs.value}')
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
			if base.kind == .ident && base.value == 'C' {
				g.write(node.value)
			} else if base.kind == .ident && (base.value in g.tc.enum_names
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
			} else if base.kind == .ident && base.value in g.modules {
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
						g.write('${g.enum_vals[ekey]}')
					} else if ekey2 in g.enum_vals {
						g.write('${g.enum_vals[ekey2]}')
					} else {
						g.write(c_name('${qname}.${node.value}'))
					}
				} else {
					g.write(c_name('${qname}.${node.value}'))
				}
			} else {
				needs_paren := base.kind !in [.ident, .selector, .call]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				if node.op == .arrow {
					g.write('->')
				} else if node.op == .dot {
					g.write('.')
				} else {
					mut is_ptr := false
					if base.kind == .ident {
						if typ := g.tc.cur_scope.lookup(base.value) {
							is_ptr = typ is types.Pointer
						}
					} else {
						resolved := g.tc.resolve_type(base_id)
						is_ptr = resolved is types.Pointer
					}
					if is_ptr {
						g.write('->')
					} else {
						g.write('.')
					}
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
				c_key := g.tc.c_type(base_type.key_type)
				c_val := g.tc.c_type(base_type.value_type)
				g.write('(*(${c_val}*)map__get(&')
				g.gen_expr(base_id)
				g.write(', &(${c_key}[]){')
				g.gen_expr(g.a.child(&node, 1))
				g.write('}, &(${c_val}[]){0}))')
			} else {
				is_array_index, is_ptr, arr_type := array_index_info(base_type)
				if is_array_index {
					c_elem := g.tc.c_type(arr_type.elem_type)
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
			g.gen_map_init(node)
		}
		.cast_expr {
			target_type := g.tc.parse_type(node.value)
			ct := g.tc.c_type(target_type)
			if node.value in g.interfaces || g.tc.qualify_name(node.value) in g.interfaces {
				g.write('(${ct}){0}')
			} else if target_type is types.SumType {
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
				if g.variant_references_sum(variant_name, target_type.name) {
					inner_ct := g.tc.c_type(g.tc.parse_type(variant_name))
					if inner.kind == .struct_init {
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
						g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(&')
						g.gen_expr(inner_id)
						g.write(', sizeof(${inner_ct}))}')
					}
				} else {
					g.write('(${ct}){.typ = ${idx}, .${field} = ')
					g.gen_expr(inner_id)
					g.write('}')
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
		g.writeln('#include <pthread.h>')
		g.writeln('#include <unistd.h>')
		g.writeln('#ifdef __APPLE__')
		g.writeln('#include <mach/mach_time.h>')
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
	g.writeln('typedef void* chan;')
	g.writeln('#define true 1')
	g.writeln('#define false 0')
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

fn (mut g FlatGen) builtin_compat_decls() {
	if !g.has_builtins {
		return
	}
	g.writeln('#define array_new(elem_size, len, cap) __new_array((len), (cap), (elem_size))')
	g.writeln('#define array_push array__push')
	g.writeln('void array__push_many(array* a, void* val, int size);')
	g.writeln('static inline void array_push_many(Array* a, Array b) { array__push_many(a, b.data, b.len); }')
	g.writeln('#define array_push_many_ptr array__push_many')
	g.writeln('#define array_get array__get')
	g.writeln('#define array_set(a, i, ...) array__set(&(a), (i), __VA_ARGS__)')
	g.writeln('array array__clone(array* a);')
	g.writeln('static inline array array_clone(array a) { return array__clone(&a); }')
	g.writeln('#define array_slice array__slice')
	g.writeln('#define array_delete array__delete')
	g.writeln('#define array_ensure_cap array__ensure_cap')
	g.writeln('#define map__get_or_set map__get_and_set')
	g.writeln('#define v_panic panic')
	g.writeln('static inline void vheap_alloc(void* p, u64 n) { (void)p; (void)n; }')
	g.writeln('static inline void vheap_free(void* p) { (void)p; }')
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
	g.writeln('static inline bool fixed_array_contains_string(const string* a, int len, string val) { for (int i = 0; i < len; i++) if (a[i].len == val.len && memcmp(a[i].str, val.str, val.len) == 0) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_u8(const u8* a, int len, u8 val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_int(const int* a, int len, int val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline string Array_str(Array a) { (void)a; return (string){(u8*)"[]", 2, 1}; }')
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
	g.writeln('#ifndef max_int')
	g.writeln('#define max_int max_i32')
	g.writeln('#endif')
	g.writeln('#ifndef min_int')
	g.writeln('#define min_int min_i32')
	g.writeln('#endif')
	g.writeln('')
}

fn (mut g FlatGen) global_decls() {
	for name, typ in g.global_types {
		if typ is types.ArrayFixed {
			c_elem := g.tc.c_type(typ.elem_type)
			len_expr := g.fixed_array_len_value(typ)
			g.writeln('${c_elem} ${c_name(name)}[${len_expr}];')
			continue
		}
		ct := g.tc.c_type(typ)
		if ct == 'void' {
			continue
		}
		if typ is types.Struct && typ.name.starts_with('C.') {
			continue
		}
		g.writeln('${ct} ${c_name(name)};')
	}
	if g.global_types.len > 0 {
		g.writeln('')
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
	if name in g.const_modules {
		g.tc.cur_module = g.const_modules[name]
	}
	val_node := g.a.nodes[int(val_id)]
	if val_node.kind == .empty {
		return
	}
	expr_str := if g.is_const_expr(val_id) {
		g.const_expr_to_string(val_id, []string{})
	} else {
		g.expr_to_string(val_id)
	}
	if expr_str.trim_space().len == 0 {
		return
	}
	v_type := g.tc.resolve_type(val_id)
	ct := g.tc.c_type(v_type)
	mut qname := c_name(name)
	if name in g.const_modules && g.const_modules[name].len > 0 && g.const_modules[name] != 'main'
		&& g.const_modules[name] != 'builtin' {
		qname = c_name('${g.const_modules[name]}.${name}')
	}
	if !g.is_const_expr(val_id) {
		if v_type is types.ArrayFixed && val_node.kind == .array_literal {
			c_elem := g.tc.c_type(v_type.elem_type)
			len_expr := g.fixed_array_len_value(v_type)
			g.writeln('${c_elem} ${qname}[${len_expr}];')
			for ci in 0 .. val_node.children_count {
				elem_id := g.a.child(&val_node, ci)
				tmp2 := g.sb
				tmp2_line_start := g.line_start
				g.sb = strings.new_builder(64)
				g.line_start = true
				g.gen_expr(elem_id)
				estr := g.sb.str()
				g.sb = tmp2
				g.line_start = tmp2_line_start
				g.runtime_inits << '\t${qname}[${ci}] = ${estr};'
			}
		} else if g.is_runtime_assignable(val_id) {
			g.writeln('${ct} ${qname};')
			g.runtime_inits << '\t${qname} = ${expr_str};'
		}
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
		g.writeln('#define ${qname} (${expr_str})')
	} else {
		g.writeln('const ${ct} ${qname} = ${expr_str};')
	}
}

fn (mut g FlatGen) precompute_consts() string {
	old_sb := g.sb
	old_line_start := g.line_start
	g.sb = strings.new_builder(1024)
	g.line_start = true
	mut emitted := map[string]bool{}
	mut deferred := []string{}
	for name, val_id in g.const_vals {
		if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
			continue
		}
		if g.const_refs_other_const(val_id) {
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
		.int_literal, .float_literal, .bool_literal, .char_literal, .enum_val, .sizeof_expr {
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
			node.value in g.const_vals
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
		.infix {
			if node.children_count >= 2 {
				lhs_type := g.tc.resolve_type(g.a.child(&node, 0))
				rhs_type := g.tc.resolve_type(g.a.child(&node, 1))
				lhs_type is types.String || rhs_type is types.String
			} else {
				false
			}
		}
		.cast_expr, .prefix, .struct_init {
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
