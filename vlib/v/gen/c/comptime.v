// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import v.ast
import v.util
import v.pref
import v.comptime

fn (mut g Gen) comptime_selector(node ast.ComptimeSelector) {
	g.expr(node.left)
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	// check for field.name
	if node.field_expr is ast.SelectorExpr {
		if node.field_expr.expr is ast.Ident {
			if node.field_expr.expr.name == g.comptime.comptime_for_field_var
				&& node.field_expr.field_name == 'name' {
				_, field_name := g.comptime.get_comptime_selector_var_type(node)
				g.write(c_name(field_name))
				return
			}
		}
	}
	g.expr(node.field_expr)
}

fn (mut g Gen) comptime_call(mut node ast.ComptimeCall) {
	if node.is_embed {
		// $embed_file('/path/to/file')
		g.gen_embed_file_init(mut node)
		return
	}
	if node.method_name == 'env' {
		// $env('ENV_VAR_NAME')
		val := util.cescaped_path(os.getenv(node.args_var))
		g.write('_SLIT("${val}")')
		return
	}
	if node.method_name == 'res' {
		if node.args_var != '' {
			g.write('${g.defer_return_tmp_var}.arg${node.args_var}')
			return
		}

		g.write('${g.defer_return_tmp_var}')
		return
	}
	if node.is_vweb {
		is_html := node.method_name == 'html'
		mut cur_line := ''

		if !is_html {
			cur_line = g.go_before_last_stmt()
		}

		ret_sym := g.table.sym(g.fn_decl.return_type)
		fn_name := g.fn_decl.name.replace('.', '__') + node.pos.pos.str()
		is_x_vweb := ret_sym.cname == 'x__vweb__Result'

		for stmt in node.vweb_tmpl.stmts {
			if stmt is ast.FnDecl {
				if stmt.name.starts_with('main.vweb_tmpl') {
					if is_html {
						g.inside_vweb_tmpl = true
						if is_x_vweb {
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
			if is_x_vweb {
				ctx_name := g.fn_decl.params[1].name
				g.writeln('x__vweb__Context_html(${ctx_name}, _tmpl_res_${fn_name});')
			} else {
				// old vweb:
				app_name := g.fn_decl.params[0].name
				g.writeln('vweb__Context_html(&${app_name}->Context, _tmpl_res_${fn_name});')
			}
			g.writeln('strings__Builder_free(&sb_${fn_name});')
			g.writeln('string_free(&_tmpl_res_${fn_name});')
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
	sym := g.table.sym(g.unwrap_generic(node.left_type))
	g.trace_autofree('// \$method call. sym="${sym.name}"')
	if node.method_name == 'method' {
		// `app.$method()`
		m := sym.find_method(g.comptime.comptime_for_method) or { return }
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

			arg.expr is ast.Ident && g.table.type_to_str(arg.typ) == '[]string'
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
					g.writeln('/* skipping ${sym.name}.${m.name} due to mismatched arguments list */')
					// g.writeln('println(_SLIT("skipping ${node.sym.name}.$m.name due to mismatched arguments list"));')
					// eprintln('info: skipping ${node.sym.name}.$m.name due to mismatched arguments list\n' +
					//'method.params: $m.params, args: $node.args\n\n')
					// verror('expected ${m.params.len-1} arguments to method ${node.sym.name}.$m.name, but got $node.args.len')
					return
				}
			}
		}

		if !g.inside_call && node.or_block.kind != .block && m.return_type.has_option_or_result() {
			g.write('(*(${g.base_type(m.return_type)}*)')
		}
		// TODO: check argument types
		g.write('${util.no_dots(sym.name)}_${g.comptime.comptime_for_method}(')

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
					g.write('*(${g.typ(m.params[i].typ)}*)array_get(')
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
				if m.params[i].typ.is_int() || m.params[i].typ.idx() == ast.bool_type_idx {
					// Gets the type name and cast the string to the type with the string_<type> function
					type_name := g.table.type_symbols[int(m.params[i].typ)].str()
					g.write('string_${type_name}(((string*)${last_arg}.data) [${idx}])')
				} else {
					g.write('((string*)${last_arg}.data) [${idx}] ')
				}
				if i < m.params.len - 1 {
					g.write(', ')
				}
			}
		}
		g.write(')')
		if !g.inside_call && node.or_block.kind != .block && m.return_type.has_option_or_result() {
			g.write('.data)')
		}
		if node.or_block.kind != .absent && m.return_type.has_option_or_result() {
			if !g.inside_assign {
				cur_line := g.go_before_last_stmt()
				tmp_var := g.new_tmp_var()
				g.write('${g.typ(m.return_type)} ${tmp_var} = ')
				g.write(cur_line)
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
			g.write('if (string__eq(${node.method_name}, _SLIT("${method.name}"))) ')
		}
		g.write('${util.no_dots(sym.name)}_${method.name}(${amp} ')
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
		res << '_SLIT("${s}")'
	}
	return res
}

fn (mut g Gen) comptime_at(node ast.AtExpr) {
	if node.kind == .vmod_file {
		val := cescape_nonascii(util.smart_quote(node.val, false))
		g.write('_SLIT("${val}")')
	} else {
		val := node.val.replace('\\', '\\\\')
		g.write('_SLIT("${val}")')
	}
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
		styp := g.typ(node.typ)
		g.writeln('${styp} ${tmp_var};')
		stmt_str.trim_space()
	} else {
		''
	}
	mut comptime_if_stmts_skip := false // don't write any statements if the condition is false
	// (so that for example windows calls don't get generated inside `$if macos` which
	// will lead to compilation errors)
	mut comptime_may_skip_else := false

	for i, branch in node.branches {
		start_pos := g.out.len
		if comptime_may_skip_else {
			continue // if we already have a known true, ignore other branches
		}
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('#else')
			comptime_if_stmts_skip = comptime_may_skip_else
		} else {
			if i == 0 {
				g.write('#if ')
			} else {
				g.write('#elif ')
			}
			comptime_if_stmts_skip, comptime_may_skip_else = g.comptime_if_cond(branch.cond,
				branch.pkg_exist)
			if !comptime_if_stmts_skip && comptime_may_skip_else {
				comptime_may_skip_else = false // if the cond is false, not skip else branch
			}
			comptime_if_stmts_skip = !comptime_if_stmts_skip
			g.writeln('')
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
						g.writeln('_result_ok(&(${g.base_type(node.typ)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.typ)}));')
						g.writeln('}')
					} else {
						g.write('\t${tmp_var} = ')
						g.stmt(last)
					}
					g.skip_stmt_pos = prev_skip_stmt_pos
					g.writeln(';')
					g.writeln('}')
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
						g.writeln('_result_ok(&(${g.base_type(node.typ)}[]) { ${tmp_var2} }, (_result*)(&${tmp_var}), sizeof(${g.base_type(node.typ)}));')
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
			if !comptime_if_stmts_skip {
				g.stmts(branch.stmts)
			}
			if should_create_scope {
				g.writeln('}')
			}
		}
	}
	g.defer_ifdef = ''
	g.writeln('#endif')
	if node.is_expr {
		g.write('${line} ${tmp_var}')
	}
}

fn (mut g Gen) get_expr_type(cond ast.Expr) ast.Type {
	match cond {
		ast.Ident {
			return if g.comptime.is_comptime_var(cond) {
				g.unwrap_generic(g.comptime.get_comptime_var_type(cond))
			} else {
				g.unwrap_generic(cond.obj.typ)
			}
		}
		ast.TypeNode {
			return g.unwrap_generic(cond.typ)
		}
		ast.SelectorExpr {
			if cond.gkind_field == .typ {
				return g.unwrap_generic(cond.name_type)
			} else {
				name := '${cond.expr}.${cond.field_name}'
				if name in g.comptime.type_map {
					return g.comptime.type_map[name]
				} else {
					return g.unwrap_generic(cond.typ)
				}
			}
		}
		else {
			return ast.void_type
		}
	}
}

// returns the value of the bool comptime expression and if next branches may be discarded
// returning `false` means the statements inside the $if can be skipped
fn (mut g Gen) comptime_if_cond(cond ast.Expr, pkg_exist bool) (bool, bool) {
	match cond {
		ast.BoolLiteral {
			g.expr(cond)
			return cond.val, true
		}
		ast.ParExpr {
			g.write('(')
			is_cond_true, may_discard := g.comptime_if_cond(cond.expr, pkg_exist)
			g.write(')')
			return is_cond_true, may_discard
		}
		ast.PrefixExpr {
			g.write(cond.op.str())
			is_cond_true, _ := g.comptime_if_cond(cond.right, pkg_exist)
			if cond.op == .not {
				if cond.right in [ast.BoolLiteral, ast.SelectorExpr] {
					return !is_cond_true, true
				}
			}
			return is_cond_true, false
		}
		ast.PostfixExpr {
			ifdef := g.comptime_if_to_ifdef((cond.expr as ast.Ident).name, true) or {
				verror(err.msg())
				return false, true
			}
			g.write('defined(${ifdef})')
			return true, false
		}
		ast.InfixExpr {
			match cond.op {
				.and, .logical_or {
					l, d1 := g.comptime_if_cond(cond.left, pkg_exist)
					g.write(' ${cond.op} ')
					r, d2 := g.comptime_if_cond(cond.right, pkg_exist)
					return if cond.op == .and { l && r } else { l || r }, d1 && d1 == d2
				}
				.key_is, .not_is {
					if cond.left in [ast.TypeNode, ast.Ident, ast.SelectorExpr]
						&& cond.right in [ast.ComptimeType, ast.TypeNode] {
						exp_type := g.get_expr_type(cond.left)
						if cond.right is ast.ComptimeType {
							is_true := g.comptime.is_comptime_type(exp_type, cond.right)
							if cond.op == .key_is {
								if is_true {
									g.write('1')
								} else {
									g.write('0')
								}
								return is_true, true
							} else {
								if is_true {
									g.write('0')
								} else {
									g.write('1')
								}
								return !is_true, true
							}
						} else {
							got_type := g.unwrap_generic((cond.right as ast.TypeNode).typ)
							got_sym := g.table.sym(got_type)

							if got_sym.kind == .interface_ && got_sym.info is ast.Interface {
								is_true := g.table.does_type_implement_interface(exp_type,
									got_type)
								if cond.op == .key_is {
									if is_true {
										g.write('1')
									} else {
										g.write('0')
									}
									return is_true, true
								} else if cond.op == .not_is {
									if is_true {
										g.write('0')
									} else {
										g.write('1')
									}
									return !is_true, true
								}
							}
							if cond.op == .key_is {
								g.write('${exp_type.idx()} == ${got_type.idx()} && ${exp_type.has_flag(.option)} == ${got_type.has_flag(.option)}')
								return exp_type == got_type, true
							} else {
								g.write('${exp_type.idx()} != ${got_type.idx()}')
								return exp_type != got_type, true
							}
						}
					}
				}
				.eq, .ne {
					// TODO Implement `$if method.args.len == 1`
					if cond.left is ast.SelectorExpr && (g.comptime.comptime_for_field_var.len > 0
						|| g.comptime.comptime_for_method.len > 0) {
						if cond.right is ast.StringLiteral {
							if cond.left.expr is ast.Ident && cond.left.field_name == 'name' {
								if g.comptime.comptime_for_method_var.len > 0
									&& cond.left.expr.name == g.comptime.comptime_for_method_var {
									is_true := if cond.op == .eq {
										g.comptime.comptime_for_method == cond.right.val
									} else {
										g.comptime.comptime_for_method != cond.right.val
									}
									if is_true {
										g.write('1')
									} else {
										g.write('0')
									}
									return is_true, true
								} else if g.comptime.comptime_for_field_var.len > 0
									&& cond.left.expr.name == g.comptime.comptime_for_field_var {
									is_true := if cond.op == .eq {
										g.comptime.comptime_for_field_value.name == cond.right.val
									} else {
										g.comptime.comptime_for_field_value.name != cond.right.val
									}
									if is_true {
										g.write('1')
									} else {
										g.write('0')
									}
									return is_true, true
								}
							}
						} else if cond.right is ast.IntegerLiteral {
							if g.comptime.is_comptime_selector_field_name(cond.left, 'indirections') {
								is_true := match cond.op {
									.eq { g.comptime.comptime_for_field_type.nr_muls() == cond.right.val.i64() }
									.ne { g.comptime.comptime_for_field_type.nr_muls() != cond.right.val.i64() }
									else { false }
								}
								if is_true {
									g.write('1')
								} else {
									g.write('0')
								}
								return is_true, true
							}
						}
					}
					if cond.left is ast.SelectorExpr || cond.right is ast.SelectorExpr {
						l, d1 := g.comptime_if_cond(cond.left, pkg_exist)
						g.write(' ${cond.op} ')
						r, d2 := g.comptime_if_cond(cond.right, pkg_exist)
						return if cond.op == .eq { l == r } else { l != r }, d1 && d1 == d2
					} else {
						g.write('1')
						return true, true
					}
				}
				.key_in, .not_in {
					if cond.left in [ast.TypeNode, ast.SelectorExpr, ast.Ident]
						&& cond.right is ast.ArrayInit {
						checked_type := g.get_expr_type(cond.left)

						for expr in cond.right.exprs {
							if expr is ast.ComptimeType {
								if g.comptime.is_comptime_type(checked_type, expr as ast.ComptimeType) {
									if cond.op == .key_in {
										g.write('1')
									} else {
										g.write('0')
									}
									return cond.op == .key_in, true
								}
							} else if expr is ast.TypeNode {
								got_type := g.unwrap_generic(expr.typ)
								if checked_type.idx() == got_type.idx()
									&& checked_type.has_flag(.option) == got_type.has_flag(.option) {
									if cond.op == .key_in {
										g.write('1')
									} else {
										g.write('0')
									}
									return cond.op == .key_in, true
								}
							}
						}
						if cond.op == .not_in {
							g.write('1')
						} else {
							g.write('0')
						}
						return cond.op == .not_in, true
					}
				}
				.gt, .lt, .ge, .le {
					if cond.left is ast.SelectorExpr && cond.right is ast.IntegerLiteral
						&& g.comptime.is_comptime_selector_field_name(cond.left, 'indirections') {
						is_true := match cond.op {
							.gt { g.comptime.comptime_for_field_type.nr_muls() > cond.right.val.i64() }
							.lt { g.comptime.comptime_for_field_type.nr_muls() < cond.right.val.i64() }
							.ge { g.comptime.comptime_for_field_type.nr_muls() >= cond.right.val.i64() }
							.le { g.comptime.comptime_for_field_type.nr_muls() <= cond.right.val.i64() }
							else { false }
						}
						if is_true {
							g.write('1')
						} else {
							g.write('0')
						}
						return is_true, true
					} else {
						return true, false
					}
				}
				else {
					return true, false
				}
			}
		}
		ast.Ident {
			ifdef := g.comptime_if_to_ifdef(cond.name, false) or { 'true' } // handled in checker
			g.write('defined(${ifdef})')
			return true, false
		}
		ast.ComptimeCall {
			g.write('${pkg_exist}')
			return true, false
		}
		ast.SelectorExpr {
			if g.comptime.comptime_for_field_var != '' && cond.expr is ast.Ident
				&& cond.expr.name == g.comptime.comptime_for_field_var
				&& cond.field_name in ['is_mut', 'is_pub', 'is_shared', 'is_atomic', 'is_option', 'is_array', 'is_map', 'is_chan', 'is_struct', 'is_alias', 'is_enum'] {
				ret_bool := g.comptime.get_comptime_selector_bool_field(cond.field_name)
				g.write(ret_bool.str())
				return ret_bool, true
			} else {
				g.write('1')
				return true, true
			}
		}
		else {
			// should be unreachable, but just in case
			g.write('1')
			return true, true
		}
	}
}

// push_new_comptime_info saves the current comptime information
fn (mut g Gen) push_new_comptime_info() {
	g.comptime_info_stack << comptime.ComptimeInfo{
		resolver: g.comptime.resolver
		table: g.comptime.table
		type_map: g.comptime.type_map.clone()
		inside_comptime_for: g.comptime.inside_comptime_for
		comptime_for_variant_var: g.comptime.comptime_for_variant_var
		comptime_for_field_var: g.comptime.comptime_for_field_var
		comptime_for_field_type: g.comptime.comptime_for_field_type
		comptime_for_field_value: g.comptime.comptime_for_field_value
		comptime_for_enum_var: g.comptime.comptime_for_enum_var
		comptime_for_method_var: g.comptime.comptime_for_method_var
		comptime_for_method: g.comptime.comptime_for_method
		comptime_for_method_ret_type: g.comptime.comptime_for_method_ret_type
	}
}

// pop_comptime_info pops the current comptime information frame
fn (mut g Gen) pop_comptime_info() {
	old := g.comptime_info_stack.pop()
	g.comptime.resolver = old.resolver
	g.comptime.table = old.table
	g.comptime.type_map = old.type_map.clone()
	g.comptime.inside_comptime_for = old.inside_comptime_for
	g.comptime.comptime_for_variant_var = old.comptime_for_variant_var
	g.comptime.comptime_for_field_var = old.comptime_for_field_var
	g.comptime.comptime_for_field_type = old.comptime_for_field_type
	g.comptime.comptime_for_field_value = old.comptime_for_field_value
	g.comptime.comptime_for_enum_var = old.comptime_for_enum_var
	g.comptime.comptime_for_method_var = old.comptime_for_method_var
	g.comptime.comptime_for_method = old.comptime_for_method
	g.comptime.comptime_for_method_ret_type = old.comptime_for_method_ret_type
}

fn (mut g Gen) resolve_comptime_type(node ast.Expr, default_type ast.Type) ast.Type {
	if (node is ast.Ident && g.comptime.is_comptime_var(node)) || node is ast.ComptimeSelector {
		return g.comptime.get_comptime_var_type(node)
	} else if node is ast.SelectorExpr && node.expr_type != 0 {
		sym := g.table.sym(g.unwrap_generic(node.expr_type))
		if f := g.table.find_field_with_embeds(sym, node.field_name) {
			return f.typ
		}
	}
	return default_type
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
		mut methods := sym.methods.filter(it.attrs.len == 0) // methods without attrs first
		methods_with_attrs := sym.methods.filter(it.attrs.len > 0) // methods with attrs second
		methods << methods_with_attrs
		if methods.len > 0 {
			g.writeln('FunctionData ${node.val_var} = {0};')
		}
		typ_vweb_result := g.table.find_type_idx('vweb.Result')
		for method in methods {
			g.push_new_comptime_info()
			// filter vweb route methods (non-generic method)
			if method.receiver_type != 0 && method.return_type == typ_vweb_result {
				rec_sym := g.table.sym(method.receiver_type)
				if rec_sym.kind == .struct_ {
					if _ := g.table.find_field_with_embeds(rec_sym, 'Context') {
						if method.generic_names.len > 0
							|| (method.params.len > 1 && method.attrs.len == 0) {
							g.pop_comptime_info()
							continue
						}
					}
				}
			}
			g.comptime.comptime_for_method = method.name
			g.comptime.comptime_for_method_var = node.val_var
			g.writeln('/* method ${i} */ {')
			g.writeln('\t${node.val_var}.name = _SLIT("${method.name}");')
			if method.attrs.len == 0 {
				g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
			} else {
				attrs := cgen_attrs(method.attrs)
				g.writeln(
					'\t${node.val_var}.attrs = new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
					attrs.join(', ') + '}));\n')
			}
			if method.params.len < 2 {
				// 0 or 1 (the receiver) args
				g.writeln('\t${node.val_var}.args = __new_array_with_default(0, 0, sizeof(MethodArgs), 0);')
			} else {
				len := method.params.len - 1
				g.write('\t${node.val_var}.args = new_array_from_c_array(${len}, ${len}, sizeof(MethodArgs), _MOV((MethodArgs[${len}]){')
				// Skip receiver arg
				for j, arg in method.params[1..] {
					typ := arg.typ.idx()
					g.write('{${typ.str()}, _SLIT("${arg.name}")}')
					if j < len - 1 {
						g.write(', ')
					}
					g.comptime.type_map['${node.val_var}.args[${j}].typ'] = typ
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
			styp := g.table.find_type_idx(sig)

			// TODO: type aliases
			ret_typ := method.return_type
			g.writeln('\t${node.val_var}.typ = ${styp};')
			g.writeln('\t${node.val_var}.return_type = ${ret_typ.idx()};')

			g.comptime.type_map['${node.val_var}.return_type'] = ret_typ
			g.comptime.type_map['${node.val_var}.typ'] = styp
			g.stmts(node.stmts)
			i++
			g.writeln('}')
			g.pop_comptime_info()
		}
	} else if node.kind == .fields {
		if sym.kind in [.struct_, .interface_] {
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
			g.push_new_comptime_info()
			for field in fields {
				g.comptime.inside_comptime_for = true
				g.comptime.comptime_for_field_var = node.val_var
				g.comptime.comptime_for_field_value = field
				g.comptime.comptime_for_field_type = field.typ
				g.writeln('/* field ${i} */ {')
				g.writeln('\t${node.val_var}.name = _SLIT("${field.name}");')
				if field.attrs.len == 0 {
					g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
				} else {
					attrs := cgen_attrs(field.attrs)
					g.writeln(
						'\t${node.val_var}.attrs = new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
						attrs.join(', ') + '}));\n')
				}
				field_sym := g.table.sym(field.typ)
				styp := field.typ
				unaliased_styp := g.table.unaliased_type(styp)

				g.writeln('\t${node.val_var}.typ = ${styp.idx()};')
				g.writeln('\t${node.val_var}.unaliased_typ = ${unaliased_styp.idx()};')
				g.writeln('\t${node.val_var}.is_pub = ${field.is_pub};')
				g.writeln('\t${node.val_var}.is_mut = ${field.is_mut};')

				g.writeln('\t${node.val_var}.is_shared = ${field.typ.has_flag(.shared_f)};')
				g.writeln('\t${node.val_var}.is_atomic = ${field.typ.has_flag(.atomic_f)};')
				g.writeln('\t${node.val_var}.is_option = ${field.typ.has_flag(.option)};')

				g.writeln('\t${node.val_var}.is_array = ${field_sym.kind in [.array, .array_fixed]};')
				g.writeln('\t${node.val_var}.is_map = ${field_sym.kind == .map};')
				g.writeln('\t${node.val_var}.is_chan = ${field_sym.kind == .chan};')
				g.writeln('\t${node.val_var}.is_struct = ${field_sym.kind == .struct_};')
				g.writeln('\t${node.val_var}.is_alias = ${field_sym.kind == .alias};')
				g.writeln('\t${node.val_var}.is_enum = ${field_sym.kind == .enum_};')

				g.writeln('\t${node.val_var}.indirections = ${field.typ.nr_muls()};')

				g.comptime.type_map['${node.val_var}.typ'] = field.typ
				g.comptime.type_map['${node.val_var}.unaliased_typ'] = unaliased_styp
				g.stmts(node.stmts)
				i++
				g.writeln('}')
			}
			g.pop_comptime_info()
		}
	} else if node.kind == .values {
		if sym.kind == .enum_ {
			if sym.info is ast.Enum {
				if sym.info.vals.len > 0 {
					g.writeln('\tEnumData ${node.val_var} = {0};')
				}
				g.push_new_comptime_info()
				for val in sym.info.vals {
					g.comptime.comptime_for_enum_var = node.val_var
					g.comptime.type_map['${node.val_var}.typ'] = node.typ

					g.writeln('/* enum vals ${i} */ {')
					g.writeln('\t${node.val_var}.name = _SLIT("${val}");')
					g.write('\t${node.val_var}.value = ')
					if g.pref.translated && node.typ.is_number() {
						g.writeln('_const_main__${val};')
					} else {
						g.writeln('${g.typ(node.typ)}__${val};')
					}
					enum_attrs := sym.info.attrs[val]
					if enum_attrs.len == 0 {
						g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
					} else {
						attrs := cgen_attrs(enum_attrs)
						g.writeln(
							'\t${node.val_var}.attrs = new_array_from_c_array(${attrs.len}, ${attrs.len}, sizeof(string), _MOV((string[${attrs.len}]){' +
							attrs.join(', ') + '}));\n')
					}
					g.stmts(node.stmts)
					g.writeln('}')
					i++
				}
				g.pop_comptime_info()
			}
		}
	} else if node.kind == .attributes {
		if sym.info is ast.Struct {
			if sym.info.attrs.len > 0 {
				g.writeln('\tStructAttribute ${node.val_var} = {0};')
			}
			for attr in sym.info.attrs {
				g.writeln('/* attribute ${i} */ {')
				g.writeln('\t${node.val_var}.name = _SLIT("${attr.name}");')
				g.writeln('\t${node.val_var}.has_arg = ${attr.has_arg};')
				g.writeln('\t${node.val_var}.arg = _SLIT("${attr.arg}");')
				g.writeln('\t${node.val_var}.kind = AttributeKind__${attr.kind};')
				g.stmts(node.stmts)
				g.writeln('}')
				i++
			}
		}
	} else if node.kind == .variants {
		if sym.info is ast.SumType {
			if sym.info.variants.len > 0 {
				g.writeln('\tVariantData ${node.val_var} = {0};')
			}
			g.comptime.inside_comptime_for = true
			g.push_new_comptime_info()
			for variant in sym.info.variants {
				g.comptime.comptime_for_variant_var = node.val_var
				g.comptime.type_map['${node.val_var}.typ'] = variant

				g.writeln('/* variant ${i} */ {')
				g.writeln('\t${node.val_var}.typ = ${variant.idx()};')
				g.stmts(node.stmts)
				g.writeln('}')
				i++
			}
			g.pop_comptime_info()
		}
	}
	g.indent--
	g.writeln('}// \$for')
}

fn (mut g Gen) comptime_if_to_ifdef(name string, is_comptime_option bool) !string {
	match name {
		// platforms/os-es:
		'windows' {
			return '_WIN32'
		}
		'ios' {
			return '__TARGET_IOS__'
		}
		'macos' {
			return '__APPLE__'
		}
		'mach' {
			return '__MACH__'
		}
		'darwin' {
			return '__DARWIN__'
		}
		'hpux' {
			return '__HPUX__'
		}
		'gnu' {
			return '__GNU__'
		}
		'qnx' {
			return '__QNX__'
		}
		'linux' {
			return '__linux__'
		}
		'serenity' {
			return '__serenity__'
		}
		'plan9' {
			return '__plan9__'
		}
		'vinix' {
			return '__vinix__'
		}
		'freebsd' {
			return '__FreeBSD__'
		}
		'openbsd' {
			return '__OpenBSD__'
		}
		'netbsd' {
			return '__NetBSD__'
		}
		'bsd' {
			return '__BSD__'
		}
		'dragonfly' {
			return '__DragonFly__'
		}
		'android' {
			return '__ANDROID__'
		}
		'termux' {
			// Note: termux is running on Android natively so __ANDROID__ will also be defined
			return '__TERMUX__'
		}
		'solaris' {
			return '__sun'
		}
		'haiku' {
			return '__HAIKU__'
		}
		//
		'js' {
			return '_VJS'
		}
		'wasm32_emscripten' {
			return '__EMSCRIPTEN__'
		}
		'native' {
			return '_VNATIVE' // when using the native backend, cgen is inactive
		}
		// compilers:
		'gcc' {
			return '__V_GCC__'
		}
		'tinyc' {
			return '__TINYC__'
		}
		'clang' {
			return '__clang__'
		}
		'mingw' {
			return '__MINGW32__'
		}
		'msvc' {
			return '_MSC_VER'
		}
		'cplusplus' {
			return '__cplusplus'
		}
		// other:
		'threads' {
			return '__VTHREADS__'
		}
		'gcboehm' {
			return '_VGCBOEHM'
		}
		'debug' {
			return '_VDEBUG'
		}
		'prod' {
			return '_VPROD'
		}
		'profile' {
			return '_VPROFILE'
		}
		'test' {
			return '_VTEST'
		}
		'glibc' {
			return '__GLIBC__'
		}
		'prealloc' {
			return '_VPREALLOC'
		}
		'no_bounds_checking' {
			return 'CUSTOM_DEFINE_no_bounds_checking'
		}
		'freestanding' {
			return '_VFREESTANDING'
		}
		// architectures:
		'amd64' {
			return '__V_amd64'
		}
		'aarch64', 'arm64' {
			return '__V_arm64'
		}
		'arm32' {
			return '__V_arm32'
		}
		'i386' {
			return '__V_x86'
		}
		// bitness:
		'x64' {
			return 'TARGET_IS_64BIT'
		}
		'x32' {
			return 'TARGET_IS_32BIT'
		}
		// endianness:
		'little_endian' {
			return 'TARGET_ORDER_IS_LITTLE'
		}
		'big_endian' {
			return 'TARGET_ORDER_IS_BIG'
		}
		'fast_math' {
			if g.pref.ccompiler_type == .msvc {
				// turned on by: `-cflags /fp:fast`
				return '_M_FP_FAST'
			}
			// turned on by: `-cflags -ffast-math`
			return '__FAST_MATH__'
		}
		else {
			if is_comptime_option
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				return 'CUSTOM_DEFINE_${name}'
			}
			return error('bad os ifdef name "${name}"') // should never happen, caught in the checker
		}
	}
	return error('none')
}
