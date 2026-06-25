module c

import strings
import v3.flat
import v3.types

// gen_expr_lvalue emits expr lvalue output for c.
fn gen_expr_lvalue(mut g FlatGen, id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .index {
		base_id := g.a.child(&node, 0)
		base_type := g.tc.resolve_type(base_id)
		if base_type is types.Map {
			c_key := g.tc.c_type(base_type.key_type)
			c_val := g.tc.c_type(base_type.value_type)
			zero := if base_type.value_type is types.Array {
				c_elem := g.tc.c_type(base_type.value_type.elem_type)
				'&(${c_val}[]){array_new(sizeof(${c_elem}), 0, 0)}'
			} else {
				'&(${c_val}[]){0}'
			}
			g.write('(*(${c_val}*)map__get_or_set(&')
			g.gen_expr(base_id)
			g.write(', &(${c_key}[]){')
			g.gen_expr(g.a.child(&node, 1))
			g.write('}, ${zero}))')
			return
		}
	}
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_split_array_append_expr_stmt(node flat.Node) bool {
	if node.kind != .infix || node.op != .pipe || node.children_count < 2 {
		return false
	}
	append_id := g.a.child(&node, 0)
	append := g.a.nodes[int(append_id)]
	if append.kind != .infix || append.op != .left_shift || append.children_count < 2 {
		return false
	}
	lhs_id := g.a.child(&append, 0)
	lhs_arr_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
	lhs_arr := array_like_type(lhs_arr_type) or { return false }
	lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
	amp := if lhs_is_ptr { '' } else { '&' }
	c_elem := g.tc.c_type(lhs_arr.elem_type)
	g.write('array_push(${amp}')
	gen_expr_lvalue(mut g, lhs_id)
	g.write(', &(${c_elem}[]){(')
	g.gen_expr(g.a.child(&append, 1))
	g.write(' ${g.op_str(node.op)} ')
	g.gen_expr(g.a.child(&node, 1))
	g.writeln(')});')
	return true
}

// gen_node emits node output for c.
fn (mut g FlatGen) gen_node(id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.fn_decl, .c_fn_decl {
			return
		}
		.expr_stmt {
			child_id := g.a.child(&node, 0)
			if int(child_id) < 0 || int(child_id) >= g.a.nodes.len {
				return
			}
			child := g.a.nodes[int(child_id)]
			if g.is_runtime_array_flags_stmt(child_id) {
				return
			}
			if child.kind == .or_expr {
				g.gen_or_expr_stmt(child)
				return
			} else if g.gen_split_array_append_expr_stmt(child) {
				return
			} else if child.kind == .infix && child.op == .left_shift {
				lhs_id := g.a.child(&child, 0)
				if child.value == 'push_many' {
					g.gen_array_push_many_stmt(lhs_id, g.a.child(&child, 1))
				} else if child.value == 'push' {
					push_rhs_id := g.a.child(&child, 1)
					mut c_elem := if child.typ.len > 0 {
						g.tc.c_type(g.tc.parse_type(child.typ))
					} else {
						'string'
					}
					lhs_arr_type := types.unwrap_pointer(g.usable_expr_type(lhs_id))
					if lhs_arr := array_like_type(lhs_arr_type) {
						push_rhs_clean := types.unwrap_pointer(g.usable_expr_type(push_rhs_id))
						if rhs_arr := array_like_type(push_rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, push_rhs_id)
								return
							}
						}
						c_elem = g.tc.c_type(lhs_arr.elem_type)
					}
					lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
					amp := if lhs_is_ptr { '' } else { '&' }
					g.write('array_push(${amp}')
					gen_expr_lvalue(mut g, lhs_id)
					g.write(', &(${c_elem}[]){')
					if lhs_arr := array_like_type(lhs_arr_type) {
						g.gen_expr_with_expected_type(push_rhs_id, lhs_arr.elem_type)
					} else {
						g.gen_expr(push_rhs_id)
					}
					g.writeln('});')
				} else {
					lhs_type := g.usable_expr_type(lhs_id)
					clean := types.unwrap_pointer(lhs_type)
					if lhs_arr := array_like_type(clean) {
						rhs_id := g.a.child(&child, 1)
						rhs_type := g.usable_expr_type(rhs_id)
						rhs_clean := types.unwrap_pointer(rhs_type)
						if rhs_arr := array_like_type(rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, rhs_id)
							} else {
								c_elem := g.tc.c_type(lhs_arr.elem_type)
								lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
								amp := if lhs_is_ptr { '' } else { '&' }
								g.write('array_push(${amp}')
								gen_expr_lvalue(mut g, lhs_id)
								g.write(', &(${c_elem}[]){')
								g.gen_expr_with_expected_type(rhs_id, lhs_arr.elem_type)
								g.writeln('});')
							}
						} else if rhs_fixed := array_fixed_type(rhs_clean) {
							if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
								&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
								g.gen_array_push_many_stmt(lhs_id, rhs_id)
							} else {
								c_elem := g.tc.c_type(lhs_arr.elem_type)
								lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
								amp := if lhs_is_ptr { '' } else { '&' }
								g.write('array_push(${amp}')
								gen_expr_lvalue(mut g, lhs_id)
								g.write(', &(${c_elem}[]){')
								g.gen_expr_with_expected_type(rhs_id, lhs_arr.elem_type)
								g.writeln('});')
							}
						} else {
							c_elem := g.tc.c_type(lhs_arr.elem_type)
							lhs_is_ptr := g.tc.resolve_type(lhs_id) is types.Pointer
							amp := if lhs_is_ptr { '' } else { '&' }
							g.write('array_push(${amp}')
							gen_expr_lvalue(mut g, lhs_id)
							g.write(', &(${c_elem}[]){')
							g.gen_expr(rhs_id)
							g.writeln('});')
						}
					} else {
						g.gen_expr(child_id)
						g.writeln(';')
					}
				}
			} else {
				g.gen_expr(child_id)
				g.writeln(';')
			}
		}
		.decl_assign {
			g.gen_decl_assign(node)
		}
		.assign, .selector_assign {
			g.gen_assign(node)
		}
		.index_assign {
			g.gen_index_assign(node)
		}
		.return_stmt {
			if g.cur_fn_ret is types.Enum {
				g.expected_enum = g.cur_fn_ret.name
			}
			if node.children_count > 0 && g.has_pending_defers() {
				g.gen_return_with_defers(node)
				g.expected_enum = ''
				return
			}
			g.gen_all_defers()
			if node.children_count > 0 {
				ret_id := g.a.child(&node, 0)
				if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
					g.gen_default_return_stmt()
					g.expected_enum = ''
					return
				}
				ret_node := g.a.nodes[int(ret_id)]
				if ret_node.kind == .call {
					fn_n := g.a.child_node(&ret_node, 0)
					if fn_n.value == 'error' || fn_n.value == 'error_with_code' {
						if g.cur_fn_ret_is_optional {
							ct := g.optional_type_name(g.cur_fn_ret)
							g.write('return ')
							g.gen_optional_error_from_call(ct, ret_node)
							g.writeln(';')
						} else {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						}
						return
					}
				}
				if g.cur_fn_ret_is_optional {
					ct := g.optional_type_name(g.cur_fn_ret)
					base := g.cur_fn_ret_base
					if g.expr_is_optional_literal(ret_id, g.cur_fn_ret) {
						g.write('return ')
						g.gen_expr(ret_id)
						g.writeln(';')
						return
					}
					if base is types.MultiReturn && node.children_count > 1 {
						base_ct := g.tc.c_type(base)
						g.write('return (${ct}){.ok = true, .value = (${base_ct}){')
						for i in 0 .. node.children_count {
							if i > 0 {
								g.write(', ')
							}
							child_id := g.a.child(&node, i)
							if i < base.types.len {
								g.gen_expr_with_expected_type(child_id, base.types[i])
							} else {
								g.gen_expr(child_id)
							}
						}
						g.writeln('}};')
						return
					}
					if ret_node.kind == .none_expr {
						g.writeln('return (${ct}){.ok = false};')
						return
					}
					if base is types.Void {
						g.writeln('return (${ct}){.ok = false};')
					} else if base is types.ArrayFixed {
						// The optional's `.value` is a fixed-array member, which can't be set
						// in the compound literal; build via a temp + memcpy.
						g.write('return ({ ${ct} __opt = {.ok = true}; memcpy(__opt.value, ')
						g.gen_fixed_array_copy_source(ret_id, base)
						g.writeln(', sizeof(__opt.value)); __opt; });')
					} else {
						raw_expr_type := g.tc.resolve_type(ret_id)
						expr_type := g.usable_expr_type(ret_id)
						call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
						decl_ret_type := g.declared_call_return_type(ret_node)
						if g.optional_result_matches_base(raw_expr_type, base)
							|| g.optional_result_matches_base(expr_type, base)
							|| g.optional_result_matches_base(call_ret_type, base)
							|| g.optional_result_matches_base(decl_ret_type, base) {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						} else {
							mut expr_value_type := expr_type
							if expr_type is types.OptionType {
								expr_value_type = expr_type.base_type
							} else if expr_type is types.ResultType {
								expr_value_type = expr_type.base_type
							}
							base_ct := g.tc.c_type(base)
							expr_ct := g.tc.c_type(expr_value_type)
							struct_init_ct := if ret_node.kind == .struct_init {
								g.struct_init_c_type_name(ret_node.value)
							} else {
								''
							}
							if expr_ct != base_ct && struct_init_ct != base_ct
								&& !g.type_names_match(expr_value_type, base)
								&& !g.type_can_wrap_as_sum(expr_value_type, base)
								&& !g.types_numeric_compatible(expr_value_type, base)
								&& !g.call_constructs_type(ret_id, base)
								&& expr_value_type !is types.Primitive
								&& expr_value_type !is types.Unknown {
								g.writeln('return (${ct}){.ok = false};')
							} else {
								g.write('return (${ct}){.ok = true, .value = ')
								g.gen_expr_with_expected_type(ret_id, base)
								g.writeln('};')
							}
						}
					}
				} else if g.cur_fn_ret is types.MultiReturn {
					if node.children_count > 1 {
						ct := g.tc.c_type(g.cur_fn_ret)
						mr_types := g.cur_fn_ret.types
						mut has_fixed := false
						for mt in mr_types {
							if mt is types.ArrayFixed {
								has_fixed = true
								break
							}
						}
						if has_fixed {
							// A multi-return with a fixed-array member can't be built by a
							// positional compound literal (array members need memcpy).
							tmp := g.tmp_name()
							g.write('return ({ ${ct} ${tmp} = {0};')
							for i in 0 .. node.children_count {
								if i < mr_types.len && mr_types[i] is types.ArrayFixed {
									g.write(' memcpy(${tmp}.arg${i}, ')
									g.gen_fixed_array_copy_source(g.a.child(&node, i), mr_types[i])
									g.write(', sizeof(${tmp}.arg${i}));')
								} else {
									g.write(' ${tmp}.arg${i} = ')
									g.gen_expr(g.a.child(&node, i))
									g.write(';')
								}
							}
							g.writeln(' ${tmp}; });')
							g.expected_enum = ''
							return
						}
						g.write('return (${ct}){')
						for i in 0 .. node.children_count {
							if i > 0 {
								g.write(', ')
							}
							g.gen_expr(g.a.child(&node, i))
						}
						g.writeln('};')
					} else {
						expr_type := g.usable_expr_type(ret_id)
						if expr_type is types.MultiReturn {
							g.write('return ')
							g.gen_expr(ret_id)
							g.writeln(';')
						} else {
							ct := g.tc.c_type(g.cur_fn_ret)
							g.write('return (${ct}){')
							g.gen_expr(ret_id)
							g.writeln('};')
						}
					}
				} else if ret_node.kind == .assoc {
					g.gen_return_assoc(ret_node)
				} else if g.cur_fn_ret is types.ArrayFixed
					&& g.tc.c_type(g.cur_fn_ret) in g.fixed_array_ret_wrappers {
					g.write('return ')
					g.gen_fixed_array_return_wrap(g.cur_fn_ret, ret_id)
					g.writeln(';')
				} else {
					g.write('return ')
					// Most interface returns are already boxed by the transform pass into
					// a `(Iface){._typ = N, ._object = ...}` literal, in which case
					// gen_interface_value_expr is a no-op (the value is already an
					// interface) and we emit it directly. IError is intentionally left
					// unboxed by the transform, so box the concrete error here. Never emit
					// a zeroed `(Iface){0}` — that drops `_typ`/`_object` and makes every
					// dispatch through the returned interface panic as "not implemented".
					if g.cur_fn_ret is types.Interface {
						if !g.gen_interface_value_expr(ret_id, g.cur_fn_ret) {
							g.gen_expr(ret_id)
						}
					} else {
						g.gen_expr(ret_id)
					}
					g.writeln(';')
				}
			} else {
				g.gen_default_return_stmt()
			}
			g.expected_enum = ''
		}
		.defer_stmt {
			if node.value == 'function' {
				if count_name := g.fn_defer_counts[int(id)] {
					g.writeln('${count_name}++;')
				}
				g.fn_defers << id
			} else {
				g.defers << g.a.child(&node, 0)
			}
		}
		.for_stmt {
			g.gen_for(node)
		}
		.for_in_stmt {
			g.gen_for_in(node)
		}
		.break_stmt {
			if node.value.len > 0 {
				g.writeln('goto ${c_name(node.value)}_break;')
			} else {
				g.writeln('break;')
			}
		}
		.continue_stmt {
			if node.value.len > 0 {
				g.writeln('goto ${c_name(node.value)}_continue;')
			} else {
				g.writeln('continue;')
			}
		}
		.block {
			g.writeln('{')
			g.tc.push_scope()
			defer_start := g.defers.len
			g.indent++
			for i in 0 .. node.children_count {
				g.gen_node(g.a.child(&node, i))
			}
			g.gen_defers_from(defer_start)
			g.trim_defers(defer_start)
			g.indent--
			g.tc.pop_scope()
			g.writeln('}')
		}
		.if_expr {
			g.gen_if(node)
		}
		.assert_stmt {
			g.write('if (!(')
			g.gen_expr(g.a.child(&node, 0))
			g.writeln(')) {')
			g.indent++
			g.writeln('fprintf(stderr, "assert failed\\n");')
			g.writeln('exit(1);')
			g.indent--
			g.writeln('}')
		}
		.goto_stmt {
			g.writeln('goto ${c_name(node.value)};')
		}
		.label_stmt {
			old_indent := g.indent
			g.indent = 0
			g.writeln('${c_name(node.value)}: ;')
			g.indent = old_indent
		}
		.empty, .asm_stmt {}
		else {
			// NOTE: match_stmt is intentionally absent — the transformer lowers every
			// match into an if/else-if chain (see transform.lower_match_stmts), so the
			// backend never sees one. Match lowering lives in the transformer, not here.
			eprintln('gen_node: unsupported node kind: ${node.kind}')
		}
	}
}

// has_pending_defers reports whether has pending defers applies in c.
fn (g &FlatGen) has_pending_defers() bool {
	return g.defers.len > 0 || g.fn_defers.len > 0
}

// gen_return_with_defers emits return with defers output for c.
fn (mut g FlatGen) gen_return_with_defers(node flat.Node) {
	ret_id := g.a.child(&node, 0)
	if int(ret_id) < 0 || int(ret_id) >= g.a.nodes.len {
		g.gen_all_defers()
		g.gen_default_return_stmt()
		return
	}
	ret_node := g.a.nodes[int(ret_id)]
	if ret_node.kind == .assoc {
		tmp := g.tmp_name()
		g.gen_assoc_return_tmp(ret_node, tmp)
		g.gen_all_defers()
		g.writeln('return ${tmp};')
		return
	}
	if g.cur_fn_ret is types.ArrayFixed && g.tc.c_type(g.cur_fn_ret) in g.fixed_array_ret_wrappers {
		wrapper := fixed_array_ret_wrapper_name(g.tc.c_type(g.cur_fn_ret))
		tmp := g.tmp_name()
		g.write('${wrapper} ${tmp} = ')
		g.gen_fixed_array_return_wrap(g.cur_fn_ret, ret_id)
		g.writeln(';')
		g.gen_all_defers()
		g.writeln('return ${tmp};')
		return
	}
	ct := g.return_c_type()
	expr := g.return_expr_string(node, ret_id, ret_node, ct)
	tmp := g.tmp_name()
	g.writeln('${ct} ${tmp} = ${expr};')
	g.gen_all_defers()
	g.writeln('return ${tmp};')
}

// gen_fixed_array_return_wrap emits a fixed-array return value wrapped in its
// return-wrapper struct: `({ Wrapper __fa_ret; memcpy(__fa_ret.ret_arr, <expr>,
// sizeof(...)); __fa_ret; })`. C cannot return raw arrays, so the array is copied
// into the wrapper's `ret_arr` field and the struct is returned by value.
fn (mut g FlatGen) gen_fixed_array_return_wrap(ret_type types.Type, ret_id flat.NodeId) {
	wrapper := fixed_array_ret_wrapper_name(g.tc.c_type(ret_type))
	g.write('({ ${wrapper} __fa_ret; memcpy(__fa_ret.ret_arr, ')
	g.gen_fixed_array_copy_source(ret_id, ret_type)
	g.write(', sizeof(__fa_ret.ret_arr)); __fa_ret; })')
}

fn (mut g FlatGen) gen_default_return_stmt() {
	if g.cur_fn_ret_is_optional {
		ct := g.optional_type_name(g.cur_fn_ret)
		g.writeln('return (${ct}){.ok = true};')
	} else if g.cur_fn_name == 'main' {
		g.writeln('return 0;')
	} else if g.cur_fn_ret is types.Void {
		g.writeln('return;')
	} else {
		g.write('return ')
		g.gen_default_value_for_type(g.cur_fn_ret)
		g.writeln(';')
	}
}

// return_c_type supports return c type handling for FlatGen.
fn (mut g FlatGen) return_c_type() string {
	if g.cur_fn_ret_is_optional {
		return g.optional_type_name(g.cur_fn_ret)
	}
	return g.tc.c_type(g.cur_fn_ret)
}

// return_expr_string supports return expr string handling for FlatGen.
fn (mut g FlatGen) return_expr_string(node flat.Node, ret_id flat.NodeId, ret_node flat.Node, ct string) string {
	if ret_node.kind == .call {
		fn_n := g.a.child_node(&ret_node, 0)
		if fn_n.value == 'error' || fn_n.value == 'error_with_code' {
			if g.cur_fn_ret_is_optional {
				return g.optional_error_from_call_string(ct, ret_node)
			}
			return g.expr_to_string(ret_id)
		}
	}
	if g.cur_fn_ret_is_optional {
		base := g.cur_fn_ret_base
		if g.expr_is_optional_literal(ret_id, g.cur_fn_ret) {
			return g.expr_to_string(ret_id)
		}
		if base is types.MultiReturn && node.children_count > 1 {
			base_ct := g.tc.c_type(base)
			mut parts := []string{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				child_id := g.a.child(&node, i)
				if i < base.types.len {
					parts << g.expr_to_string_with_expected_type(child_id, base.types[i])
				} else {
					parts << g.expr_to_string(child_id)
				}
			}
			return '(${ct}){.ok = true, .value = (${base_ct}){${parts.join(', ')}}}'
		}
		if ret_node.kind == .none_expr {
			return '(${ct}){.ok = false}'
		}
		if base is types.Void {
			return '(${ct}){.ok = false}'
		}
		raw_expr_type := g.tc.resolve_type(ret_id)
		expr_type := g.usable_expr_type(ret_id)
		call_ret_type := g.local_fn_call_return_type(ret_id, ret_node)
		decl_ret_type := g.declared_call_return_type(ret_node)
		if g.optional_result_matches_base(raw_expr_type, base)
			|| g.optional_result_matches_base(expr_type, base)
			|| g.optional_result_matches_base(call_ret_type, base)
			|| g.optional_result_matches_base(decl_ret_type, base) {
			return g.expr_to_string(ret_id)
		}
		mut expr_value_type := expr_type
		if expr_type is types.OptionType {
			expr_value_type = expr_type.base_type
		} else if expr_type is types.ResultType {
			expr_value_type = expr_type.base_type
		}
		base_ct := g.tc.c_type(base)
		expr_ct := g.tc.c_type(expr_value_type)
		struct_init_ct := if ret_node.kind == .struct_init {
			g.struct_init_c_type_name(ret_node.value)
		} else {
			''
		}
		if expr_ct != base_ct && struct_init_ct != base_ct
			&& !g.type_names_match(expr_value_type, base)
			&& !g.type_can_wrap_as_sum(expr_value_type, base)
			&& !g.types_numeric_compatible(expr_value_type, base)
			&& !g.call_constructs_type(ret_id, base) && expr_value_type !is types.Primitive
			&& expr_value_type !is types.Unknown {
			return '(${ct}){.ok = false}'
		}
		return '(${ct}){.ok = true, .value = ${g.expr_to_string_with_expected_type(ret_id, base)}}'
	}
	if g.cur_fn_ret is types.MultiReturn {
		if node.children_count > 1 {
			mut parts := []string{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				parts << g.expr_to_string(g.a.child(&node, i))
			}
			return '(${ct}){${parts.join(', ')}}'
		}
		expr_type := g.usable_expr_type(ret_id)
		if expr_type is types.MultiReturn {
			return g.expr_to_string(ret_id)
		}
		return '(${ct}){${g.expr_to_string(ret_id)}}'
	}
	if g.cur_fn_ret is types.Interface {
		return '(${ct}){0}'
	}
	return g.expr_to_string(ret_id)
}

fn (g &FlatGen) local_fn_call_return_type(call_id flat.NodeId, call_node flat.Node) types.Type {
	if call_node.kind != .call || call_node.children_count == 0 {
		return types.Type(types.void_)
	}
	mut node_type := types.Type(types.void_)
	if name := g.tc.resolved_call_name(call_id) {
		if ret := g.tc.fn_ret_types[name] {
			return ret
		}
	}
	if call_node.typ.len > 0 {
		node_type = g.tc.parse_type(call_node.typ)
		if node_type is types.OptionType || node_type is types.ResultType {
			return node_type
		}
	}
	fn_node := g.a.child_node(&call_node, 0)
	if fn_node.kind == .selector {
		if ret := g.selector_call_return_type(fn_node) {
			return ret
		}
		return node_type
	}
	if fn_node.kind != .ident {
		return node_type
	}
	if ret := g.tc.fn_ret_types[fn_node.value] {
		return ret
	}
	cfn := c_name(fn_node.value)
	if cfn != fn_node.value {
		if ret := g.tc.fn_ret_types[cfn] {
			return ret
		}
	}
	if ret := g.fn_decl_return_type_for_call_name(fn_node.value) {
		return ret
	}
	if typ := g.tc.cur_scope.lookup(fn_node.value) {
		return fn_type_return_type(typ)
	}
	typ := g.tc.resolve_type(g.a.child(&call_node, 0))
	ret_type := fn_type_return_type(typ)
	if ret_type !is types.Void {
		return ret_type
	}
	return node_type
}

// declared_call_return_type returns the *declared* return type of a (possibly
// lowered) call's target function, preserving type aliases. Method calls are
// lowered to ident calls (`Recv__method(recv, ...)`) before codegen, and the
// call node's own `.typ` annotation has aliases resolved away (e.g. `?NodeId`
// becomes `?int`), which makes the optional C type name appear to differ from
// the callee's signature. The declared type read from `fn_ret_types`/the fn decl
// keeps the alias, so propagating `return call()` is recognised as valid.
fn (g &FlatGen) declared_call_return_type(call_node flat.Node) types.Type {
	if call_node.kind != .call || call_node.children_count == 0 {
		return types.Type(types.void_)
	}
	fn_node := g.a.child_node(&call_node, 0)
	if fn_node.kind == .selector {
		if ret := g.selector_call_return_type(fn_node) {
			return ret
		}
		return types.Type(types.void_)
	}
	if fn_node.kind != .ident {
		return types.Type(types.void_)
	}
	if ret := g.tc.fn_ret_types[fn_node.value] {
		return ret
	}
	cfn := c_name(fn_node.value)
	if cfn != fn_node.value {
		if ret := g.tc.fn_ret_types[cfn] {
			return ret
		}
	}
	if ret := g.fn_decl_return_type_for_call_name(fn_node.value) {
		return ret
	}
	return types.Type(types.void_)
}

fn (g &FlatGen) selector_call_return_type(fn_node flat.Node) ?types.Type {
	if fn_node.children_count == 0 || fn_node.value.len == 0 {
		return none
	}
	base_id := g.a.child(&fn_node, 0)
	base_type := g.tc.resolve_type(base_id)
	clean_type := types.unwrap_pointer(base_type)
	mut receiver_name := clean_type.name()
	if clean_type is types.Struct {
		receiver_name = clean_type.name
	} else if clean_type is types.Interface {
		receiver_name = clean_type.name
	} else if clean_type is types.Alias {
		receiver_name = clean_type.name
	}
	if receiver_name.len == 0 {
		return none
	}
	if decl_key := g.interface_method_signature_key(receiver_name, fn_node.value) {
		if ret := g.tc.fn_ret_types[decl_key] {
			return ret
		}
	}
	method_name := '${receiver_name}.${fn_node.value}'
	if ret := g.tc.fn_ret_types[method_name] {
		return ret
	}
	if receiver_name.contains('.') {
		short_name := receiver_name.all_after_last('.')
		short_method := '${short_name}.${fn_node.value}'
		if ret := g.tc.fn_ret_types[short_method] {
			return ret
		}
	}
	return none
}

fn (g &FlatGen) fn_decl_return_type_for_call_name(name string) ?types.Type {
	if name.len == 0 {
		return none
	}
	// Indexed in collect_gen_info (register_fn_decl_ret_type); previously this scanned
	// every AST node per call (O(n^2)) and re-mangled each decl name with c_name.
	if rt := g.fn_decl_ret_types[name] {
		return rt
	}
	cname := c_name(name)
	if cname != name {
		if rt := g.fn_decl_ret_types[cname] {
			return rt
		}
	}
	return none
}

fn fn_type_return_type(typ types.Type) types.Type {
	if typ is types.FnType {
		return typ.return_type
	}
	if typ is types.Alias {
		return fn_type_return_type(typ.base_type)
	}
	return types.Type(types.void_)
}

// optional_error_from_call_string converts optional error from call string data for c.
fn (mut g FlatGen) optional_error_from_call_string(ct string, node flat.Node) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = true
	g.gen_optional_error_from_call(ct, node)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

// expr_really_returns_optional supports expr really returns optional handling for FlatGen.
fn (g &FlatGen) expr_really_returns_optional(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind == .call {
		if fname := g.tc.resolved_call_name(id) {
			ret_type := g.tc.fn_ret_types[fname] or { return false }
			return ret_type is types.OptionType || ret_type is types.ResultType
		}
	}
	return false
}

// optional_result_matches_base supports optional result matches base handling for FlatGen.
fn (g &FlatGen) optional_result_matches_base(expr_type types.Type, base types.Type) bool {
	mut inner := types.Type(types.void_)
	if expr_type is types.OptionType {
		inner = expr_type.base_type
	} else if expr_type is types.ResultType {
		inner = expr_type.base_type
	} else {
		return false
	}
	if g.type_names_match(inner, base) {
		return true
	}
	// Aliases keep their declared name (e.g. `[]NodeId`) while `resolve_type` collapses
	// them to the underlying type (`[]int`), so a structural name comparison spuriously
	// fails. What actually matters for `return <call>;` is whether the C optional type
	// emitted for the call equals the one this function returns — compare those instead.
	return g.option_c_name_for_base(inner) == g.option_c_name_for_base(base)
}

// option_c_name_for_base returns the C optional type name used for a `?base`/`!base`
// value, mirroring optional_type_name without its side effects.
fn (g &FlatGen) option_c_name_for_base(base types.Type) string {
	if base is types.Void || base is types.Primitive || base is types.Enum {
		return 'Optional'
	}
	return 'Optional_' + g.tc.c_type(base).replace('*', 'ptr').replace(' ', '_')
}

// usable_expr_type supports usable expr type handling for FlatGen.
fn (g &FlatGen) usable_expr_type(id flat.NodeId) types.Type {
	if int(id) >= 0 && int(id) < g.a.nodes.len {
		node := g.a.nodes[int(id)]
		if node.kind == .ident {
			if typ := g.cur_param_types[node.value] {
				return typ
			}
			if typ := g.tc.cur_scope.lookup(node.value) {
				if typ !is types.Void {
					return typ
				}
			}
		}
		if node.kind == .index && node.children_count > 0 {
			base_type0 := g.usable_expr_type(g.a.child(&node, 0))
			base_type := types.unwrap_pointer(base_type0)
			is_slice := node.value == 'range'
				|| (node.children_count > 1 && g.a.child_node(&node, 1).kind == .range)
			if is_slice {
				if base_type is types.Array {
					return base_type
				}
				if base_type is types.ArrayFixed {
					return types.Type(types.Array{
						elem_type: base_type.elem_type
					})
				}
				if base_type is types.String {
					return types.Type(types.String{})
				}
			}
			if base_type is types.Array {
				return base_type.elem_type
			}
			if base_type is types.ArrayFixed {
				return base_type.elem_type
			}
			if base_type is types.Map {
				return base_type.value_type
			}
			if base_type is types.String {
				return types.Type(types.u8_)
			}
		}
	}
	if typ := g.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	return g.tc.resolve_type(id)
}

// type_names_match returns type names match data for FlatGen.
fn (g &FlatGen) type_names_match(a types.Type, b types.Type) bool {
	a_name := a.name()
	b_name := b.name()
	if a_name.len == 0 || b_name.len == 0 {
		return false
	}
	if a_name == b_name {
		return true
	}
	return a_name.all_after_last('.') == b_name.all_after_last('.')
}

// type_can_wrap_as_sum returns type can wrap as sum data for FlatGen.
fn (g &FlatGen) type_can_wrap_as_sum(actual types.Type, expected types.Type) bool {
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	if expected0 !is types.SumType {
		return false
	}
	actual0 := if actual is types.Alias { actual.base_type } else { actual }
	if actual0 is types.SumType {
		return false
	}
	sum_type := expected0 as types.SumType
	sum_name := g.resolve_sum_name(sum_type.name)
	variant := g.resolve_variant(sum_name, actual0.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	return variant in variants
}

// types_numeric_compatible supports types numeric compatible handling for FlatGen.
fn (g &FlatGen) types_numeric_compatible(a types.Type, b types.Type) bool {
	_ = g
	return (a.is_integer() || a.is_float()) && (b.is_integer() || b.is_float())
}

// call_constructs_type updates call constructs type state for FlatGen.
fn (g &FlatGen) call_constructs_type(id flat.NodeId, target types.Type) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .ident {
		return false
	}
	target_name := target.name()
	if target_name.len == 0 {
		return false
	}
	short_target := target_name.all_after_last('.')
	return fn_node.value == target_name || fn_node.value == short_target
}

// is_runtime_array_flags_stmt reports whether is runtime array flags stmt applies in c.
fn (g &FlatGen) is_runtime_array_flags_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['set', 'clear']
		|| fn_node.children_count == 0 {
		return false
	}
	flags_node := g.a.child_node(fn_node, 0)
	if flags_node.kind != .selector || flags_node.value != 'flags' || flags_node.children_count == 0 {
		return false
	}
	owner_id := g.a.child(flags_node, 0)
	owner_type := types.unwrap_pointer(g.tc.resolve_type(owner_id))
	return owner_type is types.Array || owner_type.name() == 'strings.Builder'
}

// gen_decl_assign emits decl assign output for c.
fn (mut g FlatGen) gen_decl_assign(node flat.Node) {
	if node.children_count >= 3 {
		rhs_type := g.tc.resolve_type(g.a.child(&node, 1))
		if rhs_type is types.MultiReturn {
			g.gen_multi_return_decl(node)
			return
		}
	}
	mut bad_decl_child := node.children_count % 2 == 1
	for i in 0 .. node.children_count {
		if int(g.a.child(&node, i)) < 0 {
			bad_decl_child = true
		}
	}
	if bad_decl_child {
		mut parts := []string{}
		for i in 0 .. node.children_count {
			child_id := g.a.child(&node, i)
			if int(child_id) < 0 {
				parts << '${i}:empty'
			} else {
				child := g.a.nodes[int(child_id)]
				parts << '${i}:${child.kind}:${child.value}:${child.typ}'
			}
		}
		panic('internal error: odd decl_assign in ${g.cur_fn_name}: count=${node.children_count} typ=${node.typ} value=${node.value} children=${parts.join('|')}')
	}
	decl_prefix := if node.value == 'static' { 'static ' } else { '' }
	mut i := 0
	for i < node.children_count {
		lhs_id := g.a.child(&node, i)
		rhs_id := g.a.child(&node, i + 1)
		lhs := g.a.nodes[int(lhs_id)]
		rhs := g.a.nodes[int(rhs_id)]
		lhs_is_defer_capture := lhs.kind == .ident && lhs.value in g.defer_capture_types
		if rhs.kind == .array_literal {
			elem_type := if rhs.children_count > 0 {
				g.tc.resolve_type(g.a.child(&rhs, 0))
			} else {
				types.Type(types.int_)
			}
			if !lhs_is_defer_capture {
				g.write('${decl_prefix}Array ')
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_array_literal_value(rhs, elem_type)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, types.Type(types.Array{
					elem_type: elem_type
				}))
			}
		} else if rhs.kind == .or_expr {
			g.gen_decl_or_expr(lhs, rhs)
		} else if rhs.kind == .array_init {
			raw_init_type := g.tc.parse_type(rhs.value)
			init_type := raw_init_type
			if init_type is types.ArrayFixed {
				c_elem := g.tc.c_type(init_type.elem_type)
				lhs_str := g.decl_lhs_str(lhs_id)
				len_expr := g.fixed_array_len_expr(rhs.value, init_type.len)
				if len_expr == '0' {
					if !lhs_is_defer_capture {
						g.writeln('${decl_prefix}${c_elem} ${lhs_str}[0];')
					}
				} else {
					if lhs_is_defer_capture {
						g.writeln('memset(${lhs_str}, 0, sizeof(${lhs_str}));')
					} else {
						g.writeln('${decl_prefix}${c_elem} ${lhs_str}[${len_expr}] = {0};')
					}
				}
				if lhs.kind == .ident {
					g.tc.cur_scope.insert(lhs.value, raw_init_type)
				}
			} else {
				c_elem := g.tc.c_type(init_type)
				mut init_len := '0'
				mut init_cap := '0'
				mut init_val := ''
				for j in 0 .. rhs.children_count {
					child := g.a.child_node(&rhs, j)
					if child.kind == .field_init {
						if child.value == 'len' {
							init_len = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'cap' {
							init_cap = g.expr_to_string(g.a.child(child, 0))
						} else if child.value == 'init' {
							init_val = g.expr_to_string(g.a.child(child, 0))
						}
					}
				}
				lhs_str := g.decl_lhs_str(lhs_id)
				if lhs_is_defer_capture {
					g.writeln('${lhs_str} = array_new(sizeof(${c_elem}), ${init_len}, ${init_cap});')
				} else {
					g.writeln('${decl_prefix}Array ${lhs_str} = array_new(sizeof(${c_elem}), ${init_len}, ${init_cap});')
				}
				if init_val.len > 0 {
					g.writeln('for (int _ai = 0; _ai < ${lhs_str}.len; _ai++) ((${c_elem}*)${lhs_str}.data)[_ai] = ${init_val};')
				}
				if lhs.kind == .ident {
					g.tc.cur_scope.insert(lhs.value, types.Type(types.Array{
						elem_type: init_type
					}))
				}
			}
		} else if rhs.kind == .map_init {
			v_type := g.tc.resolve_type(rhs_id)
			c_typ := g.tc.c_type(v_type)
			if !lhs_is_defer_capture {
				g.write('${decl_prefix}${c_typ} ')
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_expr_with_expected_type(rhs_id, v_type)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, v_type)
			}
			if rhs.children_count > 0 {
				if v_type is types.Map {
					c_key := g.tc.c_type(v_type.key_type)
					c_val := g.tc.c_type(v_type.value_type)
					for j := 0; j < rhs.children_count; j += 2 {
						g.write('map__set(&')
						g.gen_decl_lhs(lhs_id)
						g.write(', &(${c_key}[]){')
						g.gen_expr(g.a.child(&rhs, j))
						g.write('}, &(${c_val}[]){')
						g.gen_expr(g.a.child(&rhs, j + 1))
						g.writeln('});')
					}
				}
			}
		} else {
			v_type := if node.typ.len > 0 {
				g.tc.parse_type(node.typ)
			} else if rhs.kind == .if_expr {
				g.if_expr_type(&rhs)
			} else {
				g.usable_expr_type(rhs_id)
			}
			ct0 := g.tc.c_type(v_type)
			ct := if v_type is types.OptionType || v_type is types.ResultType {
				g.optional_type_name(v_type)
			} else {
				ct0
			}
			if v_type is types.ArrayFixed {
				// A fixed array cannot be initialized from another array value
				// (`T x[N] = expr` is illegal); declare then memcpy.
				lhs_str := g.decl_lhs_str(lhs_id)
				if !lhs_is_defer_capture {
					g.writeln('${decl_prefix}${ct} ${lhs_str};')
				}
				g.write('memcpy(${lhs_str}, ')
				g.gen_fixed_array_copy_source(rhs_id, v_type)
				g.writeln(', sizeof(${lhs_str}));')
				if lhs.kind == .ident {
					g.tc.cur_scope.insert(lhs.value, v_type)
				}
				i += 2
				continue
			}
			if ct.starts_with('fn_ptr:') {
				fp_name := g.resolve_fn_ptr_type(ct)
				if !lhs_is_defer_capture {
					g.write('${decl_prefix}${fp_name} ')
				}
			} else {
				if !lhs_is_defer_capture {
					g.write('${decl_prefix}${ct} ')
				}
			}
			g.gen_decl_lhs(lhs_id)
			g.write(' = ')
			g.gen_decl_init_expr(rhs_id, rhs, v_type, ct, !lhs_is_defer_capture)
			g.writeln(';')
			if lhs.kind == .ident {
				g.tc.cur_scope.insert(lhs.value, v_type)
			}
		}
		i += 2
	}
}

// gen_decl_init_expr emits decl init expr output for c.
fn (mut g FlatGen) gen_decl_init_expr(rhs_id flat.NodeId, rhs flat.Node, v_type types.Type, c_type string, is_declaration bool) {
	if g.is_json_decode_call_expr(rhs_id) {
		g.write('(${c_type}){0}')
		return
	}
	if rhs.kind == .int_literal && rhs.value == '0' && g.is_aggregate_zero_init_type(v_type, c_type) {
		if is_declaration {
			g.write('{0}')
		} else {
			g.write('(${c_type}){0}')
		}
		return
	}
	g.gen_expr_with_expected_type(rhs_id, v_type)
}

// gen_multi_return_decl emits multi return decl output for c.
fn (mut g FlatGen) gen_multi_return_decl(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_type := g.tc.resolve_type(rhs_id)
	ct := g.tc.c_type(rhs_type)
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr_with_expected_type(rhs_id, rhs_type)
	g.writeln(';')
	num_lhs := node.children_count - 1
	mut multi_types := []types.Type{}
	if rhs_type is types.MultiReturn {
		multi_types = rhs_type.types.clone()
	}
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		field_type := if j < multi_types.len {
			g.tc.c_type(multi_types[j])
		} else {
			'int'
		}
		lhs_name := c_name(lhs.value)
		g.writeln('${field_type} ${lhs_name} = ${tmp}.arg${j};')
		if lhs.kind == .ident {
			inner := if j < multi_types.len {
				multi_types[j]
			} else {
				types.Type(types.int_)
			}
			g.tc.cur_scope.insert(lhs.value, inner)
		}
	}
}

// gen_assign emits assign output for c.
fn (mut g FlatGen) gen_assign(node flat.Node) {
	if node.children_count >= 3 {
		rhs_id := g.a.child(&node, 1)
		rhs_type := g.tc.resolve_type(rhs_id)
		if rhs_type is types.MultiReturn {
			g.gen_multi_return_assign(node)
			return
		}
	}
	mut i := 0
	for i < node.children_count {
		lhs := g.a.nodes[int(g.a.child(&node, i))]
		if lhs.kind == .ident && lhs.value == '_' {
			g.write('(void)(')
			g.gen_expr(g.a.child(&node, i + 1))
			g.writeln(');')
		} else if node.op == .left_shift_assign && lhs.kind == .ident {
			if node.value == 'push_many' {
				g.gen_array_push_many_stmt(g.a.child(&node, i), g.a.child(&node, i + 1))
			} else if node.value == 'push' {
				lhs_id := g.a.child(&node, i)
				rhs_id := g.a.child(&node, i + 1)
				lhs_arr_type := types.unwrap_pointer(g.usable_expr_type(lhs_id))
				if lhs_arr := array_like_type(lhs_arr_type) {
					push_rhs_clean := types.unwrap_pointer(g.tc.resolve_type(rhs_id))
					if rhs_arr := array_like_type(push_rhs_clean) {
						if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_arr.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					} else if rhs_fixed := array_fixed_type(push_rhs_clean) {
						if g.tc.c_type(lhs_arr.elem_type) !in ['array', 'Array']
							&& g.tc.c_type(lhs_arr.elem_type) == g.tc.c_type(rhs_fixed.elem_type) {
							g.gen_array_push_many_stmt(lhs_id, rhs_id)
							i += 2
							continue
						}
					}
					lhs_is_ptr := g.tc.resolve_type(g.a.child(&node, i)) is types.Pointer
					amp := if lhs_is_ptr { '' } else { '&' }
					c_elem := g.tc.c_type(lhs_arr.elem_type)
					g.write('array_push(${amp}${c_name(lhs.value)}, &(${c_elem}[]){')
					g.gen_expr_with_expected_type(g.a.child(&node, i + 1), lhs_arr.elem_type)
					g.writeln('});')
				} else {
					// Array appends are annotated by the transformer; an un-annotated
					// `<<=` here is the integer bit-shift-assign operator.
					g.gen_expr(g.a.child(&node, i))
					g.write(' <<= ')
					g.gen_expr(g.a.child(&node, i + 1))
					g.writeln(';')
				}
			}
		} else {
			rhs_id := g.a.child(&node, i + 1)
			rhs_node := g.a.nodes[int(rhs_id)]
			if rhs_node.kind == .or_expr {
				g.gen_assign_or_expr(node, i, rhs_node)
				i += 2
				continue
			}
			lhs_id := g.a.child(&node, i)
			if rhs_node.kind == .array_literal {
				lhs_type := types.unwrap_pointer(g.tc.resolve_type(lhs_id))
				if lhs_type is types.ArrayFixed {
					// A fixed-array field/var can't be `=`-assigned an array literal (which
					// lowers to a dynamic `Array`); memcpy the element bytes instead.
					g.write('memcpy(')
					g.gen_expr(lhs_id)
					g.write(', ')
					g.gen_fixed_array_data_arg(rhs_id, lhs_type)
					g.write(', sizeof(')
					g.gen_expr(lhs_id)
					g.writeln('));')
					i += 2
					continue
				}
				elem_type := if rhs_node.children_count > 0 {
					g.tc.resolve_type(g.a.child(&rhs_node, 0))
				} else if lhs_arr := array_like_type(lhs_type) {
					lhs_arr.elem_type
				} else {
					types.Type(types.int_)
				}
				g.gen_expr(g.a.child(&node, i))
				g.write(' = ')
				g.gen_array_literal_value(rhs_node, elem_type)
				g.writeln(';')
			} else {
				lhs_type := g.usable_expr_type(lhs_id)
				rhs_type := g.usable_expr_type(rhs_id)
				if node.op == .assign && lhs_type is types.ArrayFixed {
					// A fixed array can't be assigned with `=`; copy element bytes.
					g.write('memcpy(')
					g.gen_expr(lhs_id)
					g.write(', ')
					g.gen_fixed_array_copy_source(rhs_id, lhs_type)
					g.write(', sizeof(')
					g.gen_expr(lhs_id)
					g.writeln('));')
					i += 2
					continue
				}
				if node.op == .plus_assign && (lhs_type is types.String || rhs_type is types.String) {
					g.gen_expr(g.a.child(&node, i))
					g.write(' = string__plus(')
					g.gen_expr(g.a.child(&node, i))
					g.write(', ')
					g.gen_expr(rhs_id)
					g.writeln(');')
					i += 2
					continue
				}
				if method_name := g.assign_struct_operator_method(lhs_type, node.op) {
					g.gen_expr(lhs_id)
					g.write(' = ${c_name(method_name)}(')
					g.gen_expr(lhs_id)
					g.write(', ')
					g.gen_expr(rhs_id)
					g.writeln(');')
					i += 2
					continue
				}
				if lhs_type is types.Enum {
					g.expected_enum = lhs_type.name
				}
				if g.assign_lhs_needs_deref(g.a.child(&node, i), lhs_type, rhs_type, node.op) {
					g.write('*')
				}
				g.gen_expr(g.a.child(&node, i))
				g.write(' ${g.op_str(node.op)} ')
				g.gen_expr(rhs_id)
				g.writeln(';')
				g.expected_enum = ''
			}
		}
		i += 2
	}
}

fn (g &FlatGen) assign_struct_operator_method(lhs_type types.Type, op flat.Op) ?string {
	clean := types.unwrap_pointer(lhs_type)
	if clean !is types.Struct {
		return none
	}
	op_symbol := assign_struct_operator_symbol(op) or { return none }
	method_name := '${clean.name()}.${op_symbol}'
	if method_name in g.tc.fn_param_types || method_name in g.tc.fn_ret_types {
		return method_name
	}
	cmethod_name := c_name(method_name)
	if cmethod_name in g.tc.fn_param_types || cmethod_name in g.tc.fn_ret_types {
		return cmethod_name
	}
	return none
}

fn assign_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus_assign { return '+' }
		.minus_assign { return '-' }
		.mul_assign { return '*' }
		.div_assign { return '/' }
		.mod_assign { return '%' }
		else {}
	}

	return none
}

// assign_lhs_needs_deref supports assign lhs needs deref handling for FlatGen.
fn (g &FlatGen) assign_lhs_needs_deref(lhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type, op flat.Op) bool {
	if op != .assign {
		return false
	}
	lhs := g.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return false
	}
	if lhs_type is types.Pointer {
		return lhs_type.base_type.name() == rhs_type.name()
	}
	return false
}

// gen_multi_return_assign emits multi return assign output for c.
fn (mut g FlatGen) gen_multi_return_assign(node flat.Node) {
	rhs_id := g.a.child(&node, 1)
	rhs_type := g.tc.resolve_type(rhs_id)
	ct := g.tc.c_type(rhs_type)
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr_with_expected_type(rhs_id, rhs_type)
	g.writeln(';')
	num_lhs := node.children_count - 1
	for j in 0 .. num_lhs {
		lhs_idx := if j == 0 { 0 } else { j + 1 }
		lhs_id := g.a.child(&node, lhs_idx)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		gen_expr_lvalue(mut g, lhs_id)
		g.writeln(' = ${tmp}.arg${j};')
	}
}

// gen_decl_lhs emits decl lhs output for c.
fn (mut g FlatGen) gen_decl_lhs(id flat.NodeId) {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		g.write(c_name(node.value))
	} else {
		g.gen_expr(id)
	}
}

// decl_lhs_str supports decl lhs str handling for FlatGen.
fn (mut g FlatGen) decl_lhs_str(id flat.NodeId) string {
	node := g.a.nodes[int(id)]
	if node.kind == .ident {
		return c_name(node.value)
	}
	return g.expr_to_string(id)
}

// gen_assign_or_expr emits assign or expr output for c.
fn (mut g FlatGen) gen_assign_or_expr(node flat.Node, lhs_idx int, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	g.write('${opt_ct} ${tmp} = ')
	g.gen_expr(expr_id)
	g.writeln(';')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.gen_expr(g.a.child(&node, lhs_idx))
	g.writeln(' = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		for j in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, j)
			g.gen_node(child_id)
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}

// gen_decl_or_expr emits decl or expr output for c.
fn (mut g FlatGen) gen_decl_or_expr(lhs flat.Node, or_node flat.Node) {
	expr_id := g.a.child(&or_node, 0)
	or_body_id := g.a.child(&or_node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_decl_or_map_index(lhs, expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	val_ct, val_type := g.optional_value_ct(expr_type)
	g.tc.cur_scope.insert(lhs.value, val_type)
	g.write('${opt_ct} ${tmp} = ')
	if g.is_json_decode_call_expr(expr_id) {
		g.write('(${opt_ct}){0}')
	} else {
		g.gen_expr_with_expected_type(expr_id, expr_type)
	}
	g.writeln(';')
	g.writeln('${val_ct} ${c_name(lhs.value)};')
	g.writeln('if (${tmp}.ok) {')
	g.indent++
	g.writeln('${c_name(lhs.value)} = ${tmp}.value;')
	g.indent--
	g.writeln('} else {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if or_node.value == '!' || or_node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else if or_body.children_count > 0 {
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				inner := g.a.child_node(&child, 0)
				if inner.kind == .call && g.is_noreturn_call(inner) {
					g.gen_node(child_id)
				} else {
					g.write('${c_name(lhs.value)} = ')
					g.gen_expr(g.a.child(&child, 0))
					g.writeln(';')
				}
			} else {
				g.gen_node(child_id)
			}
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}

// gen_decl_or_map_index emits decl or map index output for c.
fn (mut g FlatGen) gen_decl_or_map_index(lhs flat.Node, expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.tc.c_type(m.key_type)
	g.tc.cur_scope.insert(lhs.value, m.value_type)
	g.write('void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.writeln('});')
	g.writeln('${c_val} ${c_name(lhs.value)};')
	g.writeln('if (${tmp}) {')
	g.indent++
	g.writeln('${c_name(lhs.value)} = *(${c_val}*)${tmp};')
	g.indent--
	g.writeln('} else {')
	g.indent++
	if or_body.children_count > 0 {
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				inner := g.a.child_node(&child, 0)
				if inner.kind == .call && g.is_noreturn_call(inner) {
					g.gen_node(child_id)
				} else {
					g.write('${c_name(lhs.value)} = ')
					g.gen_expr(g.a.child(&child, 0))
					g.writeln(';')
				}
			} else {
				g.gen_node(child_id)
			}
		}
	}
	g.indent--
	g.writeln('}')
}

fn (g &FlatGen) is_json_decode_call_expr(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call && node.children_count > 0 {
		target := g.call_target_name(g.a.child(&node, 0))
		if target in ['decode', 'json.decode', 'json2.decode'] {
			return true
		}
		if g.call_has_selector_name(g.a.child(&node, 0), 'decode') {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if g.is_json_decode_call_expr(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

// is_noreturn_call reports whether is noreturn call applies in c.
fn (g &FlatGen) is_noreturn_call(node &flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(node, 0)
	return fn_node.value in ['panic', 'exit']
}

// tmp_name supports tmp name handling for FlatGen.
fn (mut g FlatGen) tmp_name() string {
	g.tmp_count++
	return '_t${g.tmp_count}'
}

// gen_or_expr emits or expr output for c.
fn (mut g FlatGen) gen_or_expr(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	expr_node := g.a.nodes[int(expr_id)]
	if expr_node.kind == .index {
		base_type := g.tc.resolve_type(g.a.child(&expr_node, 0))
		clean := types.unwrap_pointer(base_type)
		if clean is types.Map {
			g.gen_or_map_index(expr_node, clean, or_body)
			return
		}
	}
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	val_ct, val_type := g.optional_value_ct(expr_type)
	val := g.tmp_name()
	g.write('({${opt_ct} ${tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.write('; ${val_ct} ${val}; if (${tmp}.ok) { ${val} = ${tmp}.value; } else { IError err = ${tmp}.err; (void)err; ')
	// Bind `err` (IError) in a *temporary* cgen scope so the or-body's own string
	// interpolations and selector accesses resolve `err`'s type correctly (without this
	// an `${err}` inside the or-body falls back to `int__str(err)`). The scope is popped
	// afterwards so an outer local named `err` keeps its real type — e.g.
	// `err := 1; _ := maybe() or { 0 }; println('${err}')` must still see `err` as int.
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', g.tc.parse_type('IError'))
	g.gen_or_body_value(or_body, val, val_type)
	g.tc.pop_scope()
	g.write(' } ${val};})')
}

// gen_or_body emits or body output for c.
fn (mut g FlatGen) gen_or_body(or_body flat.Node) {
	if or_body.children_count == 1 {
		last_id := g.a.child(&or_body, or_body.children_count - 1)
		last := g.a.nodes[int(last_id)]
		if last.kind == .expr_stmt {
			g.gen_expr(g.a.child(&last, 0))
		} else {
			g.gen_expr(last_id)
		}
	} else {
		g.write('({')
		for i in 0 .. or_body.children_count {
			child_id := g.a.child(&or_body, i)
			child := g.a.nodes[int(child_id)]
			if i == or_body.children_count - 1 && child.kind == .expr_stmt {
				g.gen_expr(g.a.child(&child, 0))
				g.write(';')
			} else {
				g.gen_node(child_id)
			}
		}
		g.write('})')
	}
}

fn (mut g FlatGen) gen_or_body_value(or_body flat.Node, value_name string, value_type types.Type) {
	for i in 0 .. or_body.children_count {
		child_id := g.a.child(&or_body, i)
		child := g.a.nodes[int(child_id)]
		is_last := i == or_body.children_count - 1
		if is_last && child.kind == .expr_stmt {
			expr_id := g.a.child(&child, 0)
			if g.expr_is_error_call(expr_id) && g.cur_fn_ret_is_optional {
				fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
				g.write('return ')
				g.gen_optional_error_from_call(fn_opt_ct, g.a.nodes[int(expr_id)])
				g.write(';')
			} else if g.tc.resolve_type(expr_id) is types.Void {
				// A diverging/void or-body tail (e.g. `panic(..)`/`exit(..)`) yields no
				// value; emit it as a bare statement instead of assigning void.
				g.gen_expr(expr_id)
				g.write(';')
			} else {
				g.write('${value_name} = ')
				g.gen_expr_with_expected_type(expr_id, value_type)
				g.write(';')
			}
		} else {
			g.gen_node(child_id)
		}
	}
}

fn (g &FlatGen) expr_is_error_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := g.a.child_node(&node, 0)
	return fn_node.value == 'error' || fn_node.value == 'error_with_code'
}

// gen_or_map_index emits or map index output for c.
fn (mut g FlatGen) gen_or_map_index(expr_node flat.Node, m types.Map, or_body flat.Node) {
	tmp := g.tmp_name()
	c_val := g.tc.c_type(m.value_type)
	c_key := g.tc.c_type(m.key_type)
	val := g.tmp_name()
	g.write('({void* ${tmp} = map__get_check(&')
	g.gen_expr(g.a.child(&expr_node, 0))
	g.write(', &(${c_key}[]){')
	g.gen_expr(g.a.child(&expr_node, 1))
	g.write('}); ${c_val} ${val}; if (${tmp}) { ${val} = *(${c_val}*)${tmp}; } else { ')
	g.gen_or_body_value(or_body, val, m.value_type)
	g.write(' } ${val};})')
}

// gen_or_expr_stmt emits or expr stmt output for c.
fn (mut g FlatGen) gen_or_expr_stmt(node flat.Node) {
	expr_id := g.a.child(&node, 0)
	or_body_id := g.a.child(&node, 1)
	or_body := g.a.nodes[int(or_body_id)]
	tmp := g.tmp_name()
	expr_type := g.tc.resolve_type(expr_id)
	opt_ct := g.optional_type_name(expr_type)
	g.writeln('${opt_ct} ${tmp} = ')
	g.gen_expr_with_expected_type(expr_id, expr_type)
	g.writeln(';')
	g.writeln('if (!${tmp}.ok) {')
	g.tc.push_scope()
	g.tc.cur_scope.insert('err', types.Type(types.Struct{
		name: 'IError'
	}))
	g.indent++
	g.writeln('IError err = ${tmp}.err;')
	if node.value == '!' || node.value == '?' {
		if g.cur_fn_ret_is_optional {
			fn_opt_ct := g.optional_type_name(g.cur_fn_ret)
			g.writeln('return (${fn_opt_ct}){.ok = false, .err = err};')
		} else {
			g.writeln('panic(IError__str(err));')
		}
	} else {
		for i in 0 .. or_body.children_count {
			g.gen_node(g.a.child(&or_body, i))
		}
	}
	g.indent--
	g.tc.pop_scope()
	g.writeln('}')
}
