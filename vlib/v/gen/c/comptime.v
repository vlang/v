// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import v.ast
import v.util
import v.pref
import v.type_resolver

fn (mut g Gen) comptime_selector(node ast.ComptimeSelector) {
	is_interface_field := g.table.sym(node.left_type).kind == .interface
	if is_interface_field {
		g.write('*(')
	}
	g.expr(node.left)
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	// check for field.name
	if node.is_name && node.field_expr is ast.SelectorExpr {
		if node.field_expr.expr is ast.Ident {
			if node.field_expr.expr.name == g.comptime.comptime_for_field_var {
				_, field_name := g.type_resolver.get_comptime_selector_var_type(node)
				g.write(c_name(field_name))
				if is_interface_field {
					g.write(')')
				}
				return
			}
		}
	}
	g.expr(node.field_expr)
	if is_interface_field {
		g.write(')')
	}
}

fn (mut g Gen) gen_comptime_selector(expr ast.ComptimeSelector) string {
	arrow_or_dot := if expr.left_type.is_ptr() { '->' } else { '.' }
	return '${expr.left.str()}${arrow_or_dot}${g.comptime.comptime_for_field_value.name}'
}

fn (mut g Gen) comptime_call(mut node ast.ComptimeCall) {
	if node.kind == .embed_file {
		// $embed_file('/path/to/file')
		g.gen_embed_file_init(mut node)
		return
	}
	if node.kind == .env {
		// $env('ENV_VAR_NAME')
		// TODO: deprecate after support for $d() is stable
		val := util.cescaped_path(os.getenv(node.args_var))
		g.write('_S("${val}")')
		return
	}
	if node.kind == .d {
		// $d('some_string',<default value>), affected by `-d some_string=actual_value`
		val := util.cescaped_path(node.compile_value)
		if node.result_type == ast.string_type {
			g.write('_S("${val}")')
		} else if node.result_type == ast.char_type {
			g.write("'${val}'")
		} else {
			g.write('${val}')
		}
		return
	}
	if node.kind == .res {
		if node.args_var != '' {
			g.write('${g.defer_return_tmp_var}.arg${node.args_var}')
			return
		}

		g.write('${g.defer_return_tmp_var}')
		return
	}
	if node.is_vweb {
		is_html := node.kind == .html
		mut cur_line := ''

		if !is_html {
			cur_line = g.go_before_last_stmt()
		}

		ret_sym := g.table.sym(g.fn_decl.return_type)
		fn_name := g.fn_decl.name.replace('.', '__') + node.pos.pos.str()
		is_x_vweb := ret_sym.cname == 'x__vweb__Result'
		is_veb := ret_sym.cname == 'veb__Result'

		for stmt in node.veb_tmpl.stmts {
			if stmt is ast.FnDecl {
				if stmt.name.starts_with('main.veb_tmpl') {
					if is_html {
						g.inside_vweb_tmpl = true
						if is_veb {
							g.vweb_filter_fn_name = 'veb__filter'
						} else if is_x_vweb {
							g.vweb_filter_fn_name = 'x__vweb__filter'
						} else {
							g.vweb_filter_fn_name = 'vweb__filter'
						}
					}
					// insert stmts from vweb_tmpl fn
					g.stmts(stmt.stmts.filter(it !is ast.Return))
					//
					g.inside_vweb_tmpl = false
					g.vweb_filter_fn_name = ''
					break
				}
			}
		}

		if is_html {
			// return a vweb or x.vweb html template
			if is_veb {
				ctx_name := g.fn_decl.params[1].name
				g.writeln('veb__Context_html(${ctx_name}, _tmpl_res_${fn_name});')
			} else if is_x_vweb {
				ctx_name := g.fn_decl.params[1].name
				g.writeln('x__vweb__Context_html(${ctx_name}, _tmpl_res_${fn_name});')
			} else {
				// old vweb:
				app_name := g.fn_decl.params[0].name
				g.writeln('vweb__Context_html(&${app_name}->Context, _tmpl_res_${fn_name});')
			}
			g.writeln('strings__Builder_free(&sb_${fn_name});')
			g.writeln('builtin__string_free(&_tmpl_res_${fn_name});')
		} else {
			// return $tmpl string
			g.write(cur_line)
			if g.inside_return_tmpl {
				g.write('return ')
			}
			g.write('_tmpl_res_${fn_name}')
		}
		return
	}
	left_type := g.unwrap_generic(node.left_type)
	sym := g.table.sym(left_type)
	g.trace_autofree('// \$method call. sym="${sym.name}"')
	if node.method_name == 'method' {
		// `app.$method()`
		m := sym.find_method(g.comptime.comptime_for_method.name) or { return }
		/*
		vals := m.attrs[0].split('/')
		args := vals.filter(it.starts_with(':')).map(it[1..])
		println(vals)
		for val in vals {
		}
		*/
		if g.inside_call && m.return_type == ast.void_type {
			g.error('method `${m.name}()` (no value) used as value', node.pos)
		}
		expand_strs := if node.args.len > 0 && m.params.len - 1 >= node.args.len {
			arg := node.args.last()
			param := m.params[node.args.len]

			arg.expr in [ast.IndexExpr, ast.Ident] && g.table.type_to_str(arg.typ) == '[]string'
				&& g.table.type_to_str(param.typ) != '[]string'
		} else {
			false
		}
		mut has_decompose := !m.is_variadic && node.args.any(it.expr is ast.ArrayDecompose)
		// check argument length and types
		if m.params.len - 1 != node.args.len && !expand_strs {
			if g.inside_call {
				g.error('expected ${m.params.len - 1} arguments to method ${sym.name}.${m.name}, but got ${node.args.len}',
					node.pos)
			} else {
				if !has_decompose {
					// do not generate anything if the argument lengths don't match
					g.writeln('/* skipping ${sym.name}.${m.name} due to mismatched arguments list: node.args=${node.args.len} m.params=${m.params.len} */')
					// Adding a println(_S(...)) like this breaks options
					return
				}
			}
		}

		mut has_unwrap := false
		if !g.inside_call && node.or_block.kind != .block && m.return_type.has_option_or_result() {
			if !(g.assign_ct_type != 0 && g.assign_ct_type.has_option_or_result()) {
				g.write('(*(${g.base_type(m.return_type)}*)')
				has_unwrap = true
			}
		}
		// TODO: check argument types
		g.write('${g.cc_type(left_type, false)}_${g.comptime.comptime_for_method.name}(')

		// try to see if we need to pass a pointer
		if mut node.left is ast.Ident {
			if mut node.left.obj is ast.Var {
				if m.params[0].typ.is_ptr() && !node.left.obj.typ.is_ptr() {
					g.write('&')
				}
			}
		}
		g.expr(node.left)
		if m.params.len > 1 {
			g.write(', ')
		}
		for i in 1 .. m.params.len {
			if mut node.left is ast.Ident {
				if m.params[i].name == node.left.name {
					continue
				}
			}
			if i - 1 <= node.args.len - 1 && has_decompose
				&& node.args[i - 1].expr is ast.ArrayDecompose {
				mut d_count := 0
				for d_i in i .. m.params.len {
					g.write('*(${g.styp(m.params[i].typ)}*)builtin__array_get(')
					g.expr(node.args[i - 1].expr)
					g.write(', ${d_count})')

					if d_i < m.params.len - 1 {
						g.write(', ')
					}
					d_count++
				}
				break
			} else if i - 1 < node.args.len - 1 {
				g.expr(node.args[i - 1].expr)
				g.write(', ')
			} else if !expand_strs && i == node.args.len {
				g.expr(node.args[i - 1].expr)
				break
			} else {
				// last argument; try to expand if it's []string
				idx := i - node.args.len
				last_arg := g.expr_string(node.args.last().expr)
				// t := g.table.sym(m.params[i].typ)
				// g.write('/*nr_args=${node.args.len} m.params.len=${m.params.len} i=${i} t=${t.name} ${m.params}*/')
				if m.params[i].typ.is_int() || m.params[i].typ.idx() == ast.bool_type_idx {
					// Gets the type name and cast the string to the type with the string_<type> function
					type_name := g.table.type_symbols[int(m.params[i].typ)].str()
					g.write('builtin__string_${type_name}(((string*)${last_arg}.data) [${idx}])')
				} else {
					g.write('((string*)${last_arg}.data) [${idx}] ')
				}
				if i < m.params.len - 1 {
					g.write(', ')
				}
			}
		}
		g.write(')')
		if has_unwrap {
			g.write('.data)')
		}
		if node.or_block.kind != .absent && m.return_type.has_option_or_result() {
			if !g.inside_assign {
				cur_line := g.go_before_last_stmt()
				tmp_var := g.new_tmp_var()
				g.write2('${g.styp(m.return_type)} ${tmp_var} = ', cur_line)
				g.or_block(tmp_var, node.or_block, m.return_type)
			}
		}
		return
	}
	mut j := 0
	for method in sym.methods {
		// if method.return_type != ast.void_type {
		if method.return_type != node.result_type {
			continue
		}
		if method.params.len != 1 {
			continue
		}
		// receiver := method.args[0]
		// if !p.expr_var.ptr {
		// p.error('`$p.expr_var.name` needs to be a reference')
		// }
		amp := '' // if receiver.is_mut && !p.expr_var.ptr { '&' } else { '' }
		if node.is_vweb {
			if j > 0 {
				g.write(' else ')
			}
			g.write('if (builtin__string__eq(${node.method_name}, _S("${method.name}"))) ')
		}
		g.write('${g.cc_type(left_type, false)}_${method.name}(${amp} ')
		g.expr(node.left)
		g.writeln(');')
		j++
	}
}

fn cgen_attrs(attrs []ast.Attr) []string {
	mut res := []string{cap: attrs.len}
	for attr in attrs {
		// we currently don't quote 'arg' (otherwise we could just use `s := attr.str()`)
		mut s := attr.name
		if attr.arg.len > 0 {
			s += ': ${attr.arg}'
		}
		if attr.kind == .string {
			s = escape_quotes(s)
		}
		res << '_S("${s}")'
	}
	return res
}

fn (mut g Gen) comptime_at(node ast.AtExpr) {
	if node.kind == .vmod_file {
		val := cescape_nonascii(util.smart_quote(node.val, false))
		g.write('_S("${val}")')
	} else {
		val := node.val.replace('\\', '\\\\')
		g.write('_S("${val}")')
	}
}

// gen_branch_context_string generate current branches context string.
// context include generic types, `$for`.
fn (mut g Gen) gen_branch_context_string() string {
	mut arr := []string{}

	// gen `T=int,X=string`
	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0
		&& g.cur_fn.generic_names.len == g.cur_concrete_types.len {
		for i in 0 .. g.cur_fn.generic_names.len {
			arr << g.cur_fn.generic_names[i] + '=' +
				util.strip_main_name(g.table.type_to_str(g.cur_concrete_types[i]))
		}
	}

	// gen comptime `$for`
	if g.comptime.inside_comptime_for {
		// variants
		if g.comptime.comptime_for_variant_var.len > 0 {
			variant := g.table.type_to_str(g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
				ast.no_type))
			arr << g.comptime.comptime_for_variant_var + '.typ=' + variant
		}
		// fields
		if g.comptime.comptime_for_field_var.len > 0 {
			arr << g.comptime.comptime_for_field_var + '.name=' +
				g.comptime.comptime_for_field_value.name
		}
		// values
		if g.comptime.comptime_for_enum_var.len > 0 {
			enum_var := g.table.type_to_str(g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_enum_var}.typ',
				ast.void_type))
			arr << g.comptime.comptime_for_enum_var + '.typ=' + enum_var
		}
		// attributes
		if g.comptime.comptime_for_attr_var.len > 0 {
			arr << g.comptime.comptime_for_attr_var + '.name=' +
				g.comptime.comptime_for_attr_value.name
		}
		// methods
		if g.comptime.comptime_for_method_var.len > 0 {
			arr << g.comptime.comptime_for_method_var + '.name=' +
				g.comptime.comptime_for_method.name
		}
		// args
		if g.comptime.comptime_for_method_param_var.len > 0 {
			arg_var := g.table.type_to_str(g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_method_param_var}.typ',
				ast.void_type))
			arr << g.comptime.comptime_for_method_param_var + '.typ=' + arg_var
		}
	}
	return arr.join(',')
}

fn (mut g Gen) comptime_if(node ast.IfExpr) {
	if !node.is_expr && !node.has_else && node.branches.len == 1 {
		if node.branches[0].stmts.len == 0 {
			// empty ifdef; result of target OS != conditional => skip
			return
		}
		if !g.pref.output_cross_c {
			if node.branches[0].cond is ast.Ident {
				if g.pref.os == (pref.os_from_string(node.branches[0].cond.name) or {
					pref.OS._auto
				}) {
					// Same target OS as the conditional...
					// => skip the #if defined ... #endif wrapper
					// and just generate the branch statements:
					g.indent--
					g.stmts(node.branches[0].stmts)
					g.indent++
					return
				}
			}
		}
	}
	tmp_var := g.new_tmp_var()
	is_opt_or_result := node.typ.has_option_or_result()
	line := if node.is_expr {
		stmt_str := g.go_before_last_stmt()
		g.write(util.tabs(g.indent))
		styp := g.styp(node.typ)
		g.writeln('${styp} ${tmp_var};')
		stmt_str
	} else {
		''
	}

	// save node for processing hash stmts
	// we only save the first node, when there is embedded $if
	old_curr_comptime_node := g.curr_comptime_node
	if !g.comptime.inside_comptime_if {
		g.curr_comptime_node = &node
	}
	defer {
		g.curr_comptime_node = old_curr_comptime_node
	}
	mut comptime_branch_context_str := g.gen_branch_context_string()
	mut is_true := ast.ComptTimeCondResult{}
	for i, branch in node.branches {
		g.push_new_comptime_info()
		g.comptime.inside_comptime_if = true
		start_pos := g.out.len
		// `idx_str` is composed of two parts:
		// The first part represents the current context of the branch statement, `comptime_branch_context_str`, formatted like `T=int,X=string,method.name=json`
		// The second part is the branch's id.
		// This format must match what is in `checker`.
		idx_str := comptime_branch_context_str + '|id=${branch.id}|'
		if comptime_is_true := g.table.comptime_is_true[idx_str] {
			// `g.table.comptime_is_true` are the branch condition results set by `checker`
			is_true = comptime_is_true
		} else {
			g.error('checker error: condition result idx string not found => [${idx_str}]',
				node.branches[i].cond.pos())
			return
		}
		if !node.has_else || i < node.branches.len - 1 {
			if i == 0 {
				g.write('#if ')
			} else {
				g.write('#elif ')
			}
			// directly use `checker` evaluate results
			// for `cgen`, we can use `is_true.c_str` or `is_true.value` here
			g.writeln('${is_true.c_str}')
			$if debug_comptime_branch_context ? {
				g.writeln('/* ${node.branches[i].cond} | generic=[${comptime_branch_context_str}] */')
			}
		} else {
			g.writeln('#else')
		}
		expr_str := g.out.last_n(g.out.len - start_pos).trim_space()
		if expr_str != '' {
			if g.defer_ifdef != '' {
				g.defer_ifdef += '\n' + '\t'.repeat(g.indent + 1)
			}
			g.defer_ifdef += expr_str
		}
		if node.is_expr {
			len := branch.stmts.len
			if len > 0 {
				last := branch.stmts.last() as ast.ExprStmt
				if len > 1 {
					g.indent++
					g.writeln('{')
					g.stmts(branch.stmts[..len - 1])
					g.set_current_pos_as_last_stmt_pos()
					prev_skip_stmt_pos := g.skip_stmt_pos
					g.skip_stmt_pos = true
					if is_opt_or_result {
						tmp_var2 := g.new_tmp_var()
						g.write('{ ${g.base_type(node.typ)} ${tmp_var2} = ')
						g.stmt(last)
						g.writeln('builtin___result_ok(&(${g.base_type(node.typ)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.typ)}));')
						g.writeln('}')
					} else {
						g.write('\t${tmp_var} = ')
						g.stmt(last)
					}
					g.skip_stmt_pos = prev_skip_stmt_pos
					g.writeln2(';', '}')
					g.indent--
				} else {
					g.indent++
					g.set_current_pos_as_last_stmt_pos()
					prev_skip_stmt_pos := g.skip_stmt_pos
					g.skip_stmt_pos = true
					if is_opt_or_result {
						tmp_var2 := g.new_tmp_var()
						g.write('{ ${g.base_type(node.typ)} ${tmp_var2} = ')
						g.stmt(last)
						g.writeln('builtin___result_ok(&(${g.base_type(node.typ)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.typ)}));')
						g.writeln('}')
					} else {
						g.write('${tmp_var} = ')
						g.stmt(last)
					}
					g.skip_stmt_pos = prev_skip_stmt_pos
					g.writeln(';')
					g.indent--
				}
			}
		} else {
			// Only wrap the contents in {} if we're inside a function, not on the top level scope
			should_create_scope := unsafe { g.fn_decl != 0 }
			if should_create_scope {
				g.writeln('{')
			}
			if is_true.val || g.pref.output_cross_c {
				g.stmts(branch.stmts)
			}
			if should_create_scope {
				g.writeln('}')
			}
		}
		g.pop_comptime_info()
	}
	g.defer_ifdef = ''
	g.writeln('#endif')
	if node.is_expr {
		g.write('${line}${tmp_var}')
	}
}

fn (mut g Gen) get_expr_type(cond ast.Expr) ast.Type {
	match cond {
		ast.Ident {
			return g.unwrap_generic(g.type_resolver.get_type_or_default(cond, cond.obj.typ))
		}
		ast.TypeNode {
			return g.unwrap_generic(cond.typ)
		}
		ast.SelectorExpr {
			if cond.gkind_field == .typ {
				return g.unwrap_generic(cond.name_type)
			} else if cond.gkind_field == .unaliased_typ {
				return g.table.unaliased_type(g.unwrap_generic(cond.name_type))
			} else if cond.gkind_field == .indirections {
				return ast.int_type
			} else {
				if cond.expr is ast.TypeOf {
					return g.type_resolver.typeof_field_type(g.type_resolver.typeof_type(cond.expr.expr,
						cond.name_type), cond.field_name)
				}
				name := '${cond.expr}.${cond.field_name}'
				if name in g.type_resolver.type_map {
					return g.type_resolver.get_ct_type_or_default(name, ast.void_type)
				} else {
					return g.unwrap_generic(cond.typ)
				}
			}
		}
		ast.IntegerLiteral {
			return ast.int_type
		}
		ast.BoolLiteral {
			return ast.bool_type
		}
		ast.StringLiteral {
			return ast.string_type
		}
		ast.CharLiteral {
			return ast.char_type
		}
		ast.FloatLiteral {
			return ast.f64_type
		}
		else {
			return ast.void_type
		}
	}
}

// push_new_comptime_info saves the current comptime information
fn (mut g Gen) push_new_comptime_info() {
	g.type_resolver.info_stack << type_resolver.ResolverInfo{
		saved_type_map:               g.type_resolver.type_map.clone()
		inside_comptime_for:          g.comptime.inside_comptime_for
		inside_comptime_if:           g.comptime.inside_comptime_if
		comptime_for_variant_var:     g.comptime.comptime_for_variant_var
		comptime_for_field_var:       g.comptime.comptime_for_field_var
		comptime_for_field_type:      g.comptime.comptime_for_field_type
		comptime_for_field_value:     g.comptime.comptime_for_field_value
		comptime_for_enum_var:        g.comptime.comptime_for_enum_var
		comptime_for_attr_var:        g.comptime.comptime_for_attr_var
		comptime_for_attr_value:      g.comptime.comptime_for_attr_value
		comptime_for_method_var:      g.comptime.comptime_for_method_var
		comptime_for_method:          g.comptime.comptime_for_method
		comptime_for_method_ret_type: g.comptime.comptime_for_method_ret_type
		comptime_loop_id:             g.comptime.comptime_loop_id++
	}
}

// pop_comptime_info pops the current comptime information frame
fn (mut g Gen) pop_comptime_info() {
	old := g.type_resolver.info_stack.pop()
	g.type_resolver.type_map = old.saved_type_map.clone()
	g.comptime.inside_comptime_for = old.inside_comptime_for
	g.comptime.inside_comptime_if = old.inside_comptime_if
	g.comptime.comptime_for_variant_var = old.comptime_for_variant_var
	g.comptime.comptime_for_field_var = old.comptime_for_field_var
	g.comptime.comptime_for_field_type = old.comptime_for_field_type
	g.comptime.comptime_for_field_value = old.comptime_for_field_value
	g.comptime.comptime_for_enum_var = old.comptime_for_enum_var
	g.comptime.comptime_for_attr_var = old.comptime_for_attr_var
	g.comptime.comptime_for_attr_value = old.comptime_for_attr_value
	g.comptime.comptime_for_method_var = old.comptime_for_method_var
	g.comptime.comptime_for_method = old.comptime_for_method
	g.comptime.comptime_for_method_ret_type = old.comptime_for_method_ret_type
}

fn (mut g Gen) comptime_for(node ast.ComptimeFor) {
	sym := if node.typ != g.field_data_type {
		g.table.final_sym(g.unwrap_generic(node.typ))
	} else {
		g.table.final_sym(g.comptime.comptime_for_field_type)
	}
	g.writeln('/* \$for ${node.val_var} in ${sym.name}.${node.kind.str()} */ {')
	g.indent++
	mut i := 0

	if node.kind == .methods {
		methods := sym.get_methods()
		if methods.len > 0 {
			g.writeln('FunctionData ${node.val_var} = {0};')
		}
		typ_vweb_result := g.table.find_type('vweb.Result')
		for method in methods {
			g.push_new_comptime_info()
			g.comptime.inside_comptime_for = true
			// filter vweb route methods (non-generic method)
			if method.receiver_type != 0 && method.return_type == typ_vweb_result {
				rec_sym := g.table.sym(method.receiver_type)
				if rec_sym.kind == .struct {
					if _ := g.table.find_field_with_embeds(rec_sym, 'Context') {
						if method.generic_names.len > 0
							|| (method.params.len > 1 && method.attrs.len == 0) {
							g.pop_comptime_info()
							continue
						}
					}
				}
			}
			g.comptime.comptime_for_method = unsafe { &method }
			g.comptime.comptime_for_method_var = node.val_var
			g.writeln('/* method ${i} : ${method.name} */ {')
			g.writeln('\t${node.val_var}.name = _S("${method.name}");')
			if method.attrs.len == 0 {
				g.writeln('\t${node.val_var}.attrs = builtin____new_array_with_default(0, 0, sizeof(string), 0);')
			} else {
				attrs := cgen_attrs(method.attrs)
				g.writeln(
					'\t${node.val_var}.attrs = builtin__new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
					attrs.join(', ') + '}));\n')
			}
			if method.params.len < 2 {
				// 0 or 1 (the receiver) args
				g.writeln('\t${node.val_var}.args = builtin____new_array_with_default(0, 0, sizeof(FunctionParam), 0);')
			} else {
				len := method.params.len - 1
				g.write('\t${node.val_var}.args = builtin__new_array_from_c_array(${len}, ${len}, sizeof(FunctionParam), _MOV((FunctionParam[${len}]){')
				// Skip receiver arg
				for j, arg in method.params[1..] {
					typ := arg.typ.idx()
					g.write('{${typ.str()}, _S("${arg.name}")}')
					if j < len - 1 {
						g.write(', ')
					}
					g.type_resolver.update_ct_type('${node.val_var}.args[${j}].typ', typ)
				}
				g.writeln('}));\n')
			}
			mut sig := 'fn ('
			// skip the first (receiver) arg
			for j, arg in method.params[1..] {
				// TODO: ignore mut/pts in sig for now
				typ := arg.typ.set_nr_muls(0)
				sig += g.table.sym(typ).name
				if j < method.params.len - 2 {
					sig += ', '
				}
			}
			sig += ')'
			ret_type := g.table.sym(method.return_type).name
			if ret_type != 'void' {
				sig += ' ${ret_type}'
			}
			typ := g.table.find_type(sig)

			// TODO: type aliases
			ret_typ := method.return_type
			g.writeln('\t${node.val_var}.typ = ${int(typ)};')
			g.writeln('\t${node.val_var}.return_type = ${int(ret_typ.idx())};')

			g.type_resolver.update_ct_type('${node.val_var}.return_type', ret_typ)
			g.type_resolver.update_ct_type('${node.val_var}.typ', typ)
			g.stmts(node.stmts)
			i++
			g.writeln('}')
			g.pop_comptime_info()
		}
	} else if node.kind == .fields {
		if sym.kind in [.struct, .interface] {
			fields := match sym.info {
				ast.Struct {
					sym.info.fields
				}
				ast.Interface {
					sym.info.fields
				}
				else {
					g.error('comptime field lookup is supported only for structs and interfaces, and ${sym.name} is neither',
						node.pos)
					[]ast.StructField{len: 0}
				}
			}
			if fields.len > 0 {
				g.writeln('\tFieldData ${node.val_var} = {0};')
			}
			for field in fields {
				g.push_new_comptime_info()
				g.comptime.inside_comptime_for = true
				g.comptime.comptime_for_field_var = node.val_var
				g.comptime.comptime_for_field_value = field
				g.comptime.comptime_for_field_type = field.typ
				g.writeln('/* field ${i} : ${field.name} */ {')
				g.writeln('\t${node.val_var}.name = _S("${field.name}");')
				if field.attrs.len == 0 {
					g.writeln('\t${node.val_var}.attrs = builtin____new_array_with_default(0, 0, sizeof(string), 0);')
				} else {
					attrs := cgen_attrs(field.attrs)
					g.writeln(
						'\t${node.val_var}.attrs = builtin__new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
						attrs.join(', ') + '}));\n')
				}
				field_sym := g.table.sym(field.typ)
				styp := field.typ
				unaliased_styp := g.table.unaliased_type(styp)

				g.writeln('\t${node.val_var}.typ = ${int(styp.idx())};\t// ${g.table.type_to_str(styp)}')
				g.writeln('\t${node.val_var}.unaliased_typ = ${int(unaliased_styp.idx())};\t// ${g.table.type_to_str(unaliased_styp)}')
				g.writeln('\t${node.val_var}.is_pub = ${field.is_pub};')
				g.writeln('\t${node.val_var}.is_mut = ${field.is_mut};')
				g.writeln('\t${node.val_var}.is_embed = ${field.is_embed};')

				g.writeln('\t${node.val_var}.is_shared = ${field.typ.has_flag(.shared_f)};')
				g.writeln('\t${node.val_var}.is_atomic = ${field.typ.has_flag(.atomic_f)};')
				g.writeln('\t${node.val_var}.is_option = ${field.typ.has_flag(.option)};')

				g.writeln('\t${node.val_var}.is_array = ${field_sym.kind in [.array, .array_fixed]};')
				g.writeln('\t${node.val_var}.is_map = ${field_sym.kind == .map};')
				g.writeln('\t${node.val_var}.is_chan = ${field_sym.kind == .chan};')
				g.writeln('\t${node.val_var}.is_struct = ${field_sym.kind == .struct};')
				g.writeln('\t${node.val_var}.is_alias = ${field_sym.kind == .alias};')
				g.writeln('\t${node.val_var}.is_enum = ${field_sym.kind == .enum};')

				g.writeln('\t${node.val_var}.indirections = ${field.typ.nr_muls()};')

				g.type_resolver.update_ct_type('${node.val_var}.typ', field.typ)
				g.type_resolver.update_ct_type('${node.val_var}.unaliased_typ', unaliased_styp)
				g.stmts(node.stmts)
				i++
				g.writeln('}')
				g.pop_comptime_info()
			}
		}
	} else if node.kind == .values {
		if sym.kind == .enum {
			if sym.info is ast.Enum {
				if sym.info.vals.len > 0 {
					g.writeln('\tEnumData ${node.val_var} = {0};')
				}
				for val in sym.info.vals {
					g.push_new_comptime_info()
					g.comptime.inside_comptime_for = true
					g.comptime.comptime_for_enum_var = node.val_var
					g.type_resolver.update_ct_type('${node.val_var}.typ', node.typ)

					g.writeln('/* enum vals ${i} */ {')
					g.writeln('\t${node.val_var}.name = _S("${val}");')
					g.write('\t${node.val_var}.value = ')
					if g.pref.translated && node.typ.is_number() {
						g.writeln('_const_main__${val};')
					} else {
						node_sym := g.table.sym(g.unwrap_generic(node.typ))
						if node_sym.info is ast.Alias {
							g.writeln('${g.styp(node_sym.info.parent_type)}__${val};')
						} else {
							g.writeln('${g.styp(node.typ)}__${val};')
						}
					}
					enum_attrs := sym.info.attrs[val]
					if enum_attrs.len == 0 {
						g.writeln('\t${node.val_var}.attrs = builtin____new_array_with_default(0, 0, sizeof(string), 0);')
					} else {
						attrs := cgen_attrs(enum_attrs)
						g.writeln(
							'\t${node.val_var}.attrs = builtin__new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
							attrs.join(', ') + '}));\n')
					}
					g.stmts(node.stmts)
					g.writeln('}')
					i++
					g.pop_comptime_info()
				}
			}
		}
	} else if node.kind == .attributes {
		attrs := g.table.get_attrs(sym)
		if attrs.len > 0 {
			g.writeln('\tVAttribute ${node.val_var} = {0};')

			for attr in attrs {
				g.push_new_comptime_info()
				g.comptime.inside_comptime_for = true
				g.comptime.comptime_for_attr_var = node.val_var
				g.comptime.comptime_for_attr_value = attr
				g.writeln('/* attribute ${i} : ${attr.name} */ {')
				g.writeln('\t${node.val_var}.name = _S("${attr.name}");')
				g.writeln('\t${node.val_var}.has_arg = ${attr.has_arg};')
				g.writeln('\t${node.val_var}.arg = _S("${util.smart_quote(attr.arg, false)}");')
				g.writeln('\t${node.val_var}.kind = AttributeKind__${attr.kind};')
				g.stmts(node.stmts)
				g.writeln('}')
				i++
				g.pop_comptime_info()
			}
		}
	} else if node.kind == .variants {
		if sym.info is ast.SumType {
			if sym.info.variants.len > 0 {
				g.writeln('\tVariantData ${node.val_var} = {0};')
			}
			g.comptime.inside_comptime_for = true
			for variant in sym.info.variants {
				g.push_new_comptime_info()
				g.comptime.inside_comptime_for = true
				g.comptime.comptime_for_variant_var = node.val_var
				g.type_resolver.update_ct_type('${node.val_var}.typ', variant)

				g.writeln('/* variant ${i} : ${g.table.type_to_str(variant)} */ {')
				g.writeln('\t${node.val_var}.typ = ${int(variant)};\t// ')
				g.stmts(node.stmts)
				g.writeln('}')
				i++
				g.pop_comptime_info()
			}
		}
	} else if node.kind == .params {
		func := if sym.info is ast.FnType { &sym.info.func } else { g.comptime.comptime_for_method }
		if func.params.len > 0 {
			g.writeln('\tFunctionParam ${node.val_var} = {0};')
		}
		params := if func.is_method { func.params[1..] } else { func.params }
		for param in params {
			g.push_new_comptime_info()
			g.comptime.inside_comptime_for = true
			g.comptime.comptime_for_method_param_var = node.val_var
			g.type_resolver.update_ct_type('${node.val_var}.typ', param.typ)

			g.writeln('/* method param ${i} : ${param.name} */ {')
			g.writeln('\t${node.val_var}.typ = ${int(param.typ)};\t// ${g.table.type_to_str(param.typ)}')
			g.writeln('\t${node.val_var}.name = _S("${param.name}");')
			g.stmts(node.stmts)
			g.writeln('}')
			i++
			g.pop_comptime_info()
		}
	}
	g.indent--
	g.writeln('}// \$for')
}

// comptime_selector_type computes the selector type from an comptime var
fn (mut g Gen) comptime_selector_type(node ast.SelectorExpr) ast.Type {
	if !(node.expr is ast.Ident && node.expr.ct_expr) {
		return node.expr_type
	}
	prevent_sum_type_unwrapping_once := g.prevent_sum_type_unwrapping_once
	g.prevent_sum_type_unwrapping_once = false

	mut typ := g.type_resolver.get_type(node.expr)
	if node.expr.is_auto_deref_var() {
		if node.expr is ast.Ident {
			if node.expr.obj is ast.Var {
				typ = node.expr.obj.typ
			}
		}
	}
	if g.comptime.inside_comptime_for && typ == g.enum_data_type && node.field_name == 'value' {
		// for comp-time enum.values
		return g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_enum_var}.typ',
			ast.void_type)
	}
	field_name := node.field_name
	sym := g.table.sym(typ)
	final_sym := g.table.final_sym(typ)
	if (typ.has_flag(.variadic) || final_sym.kind == .array_fixed) && field_name == 'len' {
		return ast.int_type
	}
	if sym.kind == .chan {
		if field_name == 'closed' {
			return ast.bool_type
		} else if field_name in ['len', 'cap'] {
			return ast.u32_type
		}
	}
	mut has_field := false
	mut field := ast.StructField{}
	if field_name.len > 0 && field_name[0].is_capital() && sym.info is ast.Struct
		&& sym.language == .v {
		// x.Foo.y => access the embedded struct
		for embed in sym.info.embeds {
			embed_sym := g.table.sym(embed)
			if embed_sym.embed_name() == field_name {
				return embed
			}
		}
	} else {
		if f := g.table.find_field(sym, field_name) {
			has_field = true
			field = f
		} else {
			// look for embedded field
			has_field = true
			g.table.find_field_from_embeds(sym, field_name) or { has_field = false }
		}
		if typ.has_flag(.generic) && !has_field {
			gs := g.table.sym(g.unwrap_generic(typ))
			if f := g.table.find_field(gs, field_name) {
				has_field = true
				field = f
			} else {
				// look for embedded field
				has_field = true
				g.table.find_field_from_embeds(gs, field_name) or { has_field = false }
			}
		}
	}

	if has_field {
		field_sym := g.table.sym(field.typ)
		if field_sym.kind in [.sum_type, .interface] {
			if !prevent_sum_type_unwrapping_once {
				scope_field := node.scope.find_struct_field(node.expr.str(), typ, field_name)
				if scope_field != unsafe { nil } {
					return scope_field.smartcasts.last()
				}
			}
		}
		return field.typ
	}
	if mut method := g.table.sym(g.unwrap_generic(typ)).find_method_with_generic_parent(field_name) {
		method.params = method.params[1..]
		method.name = ''
		fn_type := ast.new_type(g.table.find_or_register_fn_type(method, false, true))
		return fn_type
	}
	if sym.kind !in [.struct, .aggregate, .interface, .sum_type] {
		if sym.kind != .placeholder {
			unwrapped_sym := g.table.sym(g.unwrap_generic(typ))
			if unwrapped_sym.kind == .array_fixed && node.field_name == 'len' {
				return ast.int_type
			}
		}
	}
	return node.expr_type
}

fn (mut g Gen) comptime_match(node ast.MatchExpr) {
	tmp_var := g.new_tmp_var()
	is_opt_or_result := node.return_type.has_option_or_result()
	line := if node.is_expr {
		stmt_str := g.go_before_last_stmt()
		g.write(util.tabs(g.indent))
		styp := g.styp(node.return_type)
		g.writeln('${styp} ${tmp_var};')
		stmt_str
	} else {
		''
	}

	mut comptime_branch_context_str := g.gen_branch_context_string()
	mut is_true := ast.ComptTimeCondResult{}
	for i, branch in node.branches {
		// `idx_str` is composed of two parts:
		// The first part represents the current context of the branch statement, `comptime_branch_context_str`, formatted like `T=int,X=string,method.name=json`
		// The second part is the branch's id.
		// This format must match what is in `checker`.
		idx_str := comptime_branch_context_str + '|id=${branch.id}|'
		if comptime_is_true := g.table.comptime_is_true[idx_str] {
			// `g.table.comptime_is_true` are the branch condition results set by `checker`
			is_true = comptime_is_true
		} else {
			g.error('checker error: match branch result idx string not found => [${idx_str}]',
				branch.pos)
			return
		}
		if !branch.is_else {
			if i == 0 {
				g.write('#if ')
			} else {
				g.write('#elif ')
			}
			// directly use `checker` evaluate results
			g.writeln('${is_true.val}')
			$if debug_comptime_branch_context ? {
				g.writeln('/* | generic=[${comptime_branch_context_str}] */')
			}
		} else {
			g.writeln('#else')
		}
		if node.is_expr && !branch.is_comptime_err {
			len := branch.stmts.len
			if len > 0 {
				last := branch.stmts.last()
				if last is ast.ExprStmt {
					if len > 1 {
						g.indent++
						g.writeln('{')
						g.stmts(branch.stmts[..len - 1])
						g.set_current_pos_as_last_stmt_pos()
						prev_skip_stmt_pos := g.skip_stmt_pos
						g.skip_stmt_pos = true
						if is_opt_or_result {
							tmp_var2 := g.new_tmp_var()
							g.write('{ ${g.base_type(node.return_type)} ${tmp_var2} = ')
							g.stmt(last)
							g.writeln('builtin___result_ok(&(${g.base_type(node.return_type)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.return_type)}));')
							g.writeln('}')
						} else {
							g.write('\t${tmp_var} = ')
							g.stmt(last)
						}
						g.skip_stmt_pos = prev_skip_stmt_pos
						g.writeln2(';', '}')
						g.indent--
					} else {
						g.indent++
						g.set_current_pos_as_last_stmt_pos()
						prev_skip_stmt_pos := g.skip_stmt_pos
						g.skip_stmt_pos = true
						if is_opt_or_result {
							tmp_var2 := g.new_tmp_var()
							g.write('{ ${g.base_type(node.return_type)} ${tmp_var2} = ')
							g.stmt(last)
							g.writeln('builtin___result_ok(&(${g.base_type(node.return_type)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.return_type)}));')
							g.writeln('}')
						} else {
							g.write('${tmp_var} = ')
							g.stmt(last)
						}
						g.skip_stmt_pos = prev_skip_stmt_pos
						g.writeln(';')
						g.indent--
					}
				} else if last is ast.Return {
					if last.exprs.len > 0 {
						g.write('${tmp_var} = ')
						g.expr(last.exprs[0])
						g.writeln(';')
					}
				}
			}
		} else {
			if is_true.val || g.pref.output_cross_c {
				g.stmts(branch.stmts)
			}
		}
	}
	g.writeln('#endif')
	if node.is_expr {
		g.write('${line}${tmp_var}')
	}
}
