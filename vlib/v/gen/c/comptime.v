// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import os
import v.ast
import v.util
import v.pref

fn (mut g Gen) get_comptime_selector_var_type(node ast.ComptimeSelector) (ast.StructField, string) {
	field_name := g.comptime_for_field_value.name
	left_sym := g.table.sym(g.unwrap_generic(node.left_type))
	field := g.table.find_field_with_embeds(left_sym, field_name) or {
		g.error('`${node.left}` has no field named `${field_name}`', node.left.pos())
	}
	return field, field_name
}

fn (mut g Gen) get_comptime_selector_bool_field(field_name string) bool {
	field := g.comptime_for_field_value
	field_typ := g.comptime_for_field_type
	field_sym := g.table.sym(g.unwrap_generic(g.comptime_for_field_type))

	match field_name {
		'is_pub' { return field.is_pub }
		'is_mut' { return field.is_mut }
		'is_shared' { return field_typ.has_flag(.shared_f) }
		'is_atomic' { return field_typ.has_flag(.atomic_f) }
		'is_optional' { return field.typ.has_flag(.optional) }
		'is_array' { return field_sym.kind in [.array, .array_fixed] }
		'is_map' { return field_sym.kind == .map }
		'is_chan' { return field_sym.kind == .chan }
		'is_struct' { return field_sym.kind == .struct_ }
		'is_alias' { return field_sym.kind == .alias }
		else { return false }
	}
}

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
			if node.field_expr.expr.name == g.comptime_for_field_var
				&& node.field_expr.field_name == 'name' {
				_, field_name := g.get_comptime_selector_var_type(node)
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
	if node.is_vweb {
		is_html := node.method_name == 'html'
		mut cur_line := ''

		if !is_html {
			cur_line = g.go_before_stmt(0)
		}

		for stmt in node.vweb_tmpl.stmts {
			if stmt is ast.FnDecl {
				// insert stmts from vweb_tmpl fn
				if stmt.name.starts_with('main.vweb_tmpl') {
					if is_html {
						g.inside_vweb_tmpl = true
					}
					g.stmts(stmt.stmts.filter(it !is ast.Return))
					g.inside_vweb_tmpl = false
					break
				}
			}
		}

		fn_name := g.fn_decl.name.replace('.', '__') + node.pos.pos.str()
		if is_html {
			// return vweb html template
			app_name := g.fn_decl.params[0].name
			g.writeln('vweb__Context_html(&${app_name}->Context, _tmpl_res_${fn_name}); strings__Builder_free(&sb_${fn_name}); string_free(&_tmpl_res_${fn_name});')
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
		m := sym.find_method(g.comptime_for_method) or { return }
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
		// check argument length and types
		if m.params.len - 1 != node.args.len && !expand_strs {
			if g.inside_call {
				g.error('expected ${m.params.len - 1} arguments to method ${sym.name}.${m.name}, but got ${node.args.len}',
					node.pos)
			} else {
				// do not generate anything if the argument lengths don't match
				g.writeln('/* skipping ${sym.name}.${m.name} due to mismatched arguments list */')
				// g.writeln('println(_SLIT("skipping ${node.sym.name}.$m.name due to mismatched arguments list"));')
				// eprintln('info: skipping ${node.sym.name}.$m.name due to mismatched arguments list\n' +
				//'method.params: $m.params, args: $node.args\n\n')
				// verror('expected ${m.params.len-1} arguments to method ${node.sym.name}.$m.name, but got $node.args.len')
			}
			return
		}

		if !g.inside_call && (m.return_type.has_flag(.optional) || m.return_type.has_flag(.result)) {
			g.write('(*(${g.base_type(m.return_type)}*)')
		}
		// TODO: check argument types
		g.write('${util.no_dots(sym.name)}_${g.comptime_for_method}(')

		// try to see if we need to pass a pointer
		if node.left is ast.Ident {
			if node.left.obj is ast.Var {
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
			if node.left is ast.Ident {
				if m.params[i].name == node.left.name {
					continue
				}
			}
			if i - 1 < node.args.len - 1 {
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
		if !g.inside_call && (m.return_type.has_flag(.optional) || m.return_type.has_flag(.result)) {
			g.write('.data)')
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
	line := if node.is_expr {
		stmt_str := g.go_before_stmt(0)
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
				g.defer_ifdef += '\r\n' + '\t'.repeat(g.indent + 1)
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
					g.write('\t${tmp_var} = ')
					g.stmt(last)
					g.skip_stmt_pos = prev_skip_stmt_pos
					g.writeln(';')
					g.writeln('}')
					g.indent--
				} else {
					g.indent++
					g.set_current_pos_as_last_stmt_pos()
					prev_skip_stmt_pos := g.skip_stmt_pos
					g.skip_stmt_pos = true
					g.write('${tmp_var} = ')
					g.stmt(last)
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
					left := cond.left
					mut name := ''
					if left is ast.TypeNode && cond.right is ast.ComptimeType {
						checked_type := g.unwrap_generic(left.typ)
						is_true := g.table.is_comptime_type(checked_type, cond.right)
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
					}
					mut exp_type := ast.Type(0)
					got_type := (cond.right as ast.TypeNode).typ
					// Handle `$if x is Interface {`
					// mut matches_interface := 'false'
					if left is ast.TypeNode && cond.right is ast.TypeNode
						&& g.table.sym(got_type).kind == .interface_ {
						// `$if Foo is Interface {`
						interface_sym := g.table.sym(got_type)
						if interface_sym.info is ast.Interface {
							// q := g.table.sym(interface_sym.info.types[0])
							checked_type := g.unwrap_generic(left.typ)
							// TODO PERF this check is run twice (also in the checker)
							// store the result in a field
							is_true := g.table.does_type_implement_interface(checked_type,
								got_type)
							// true // exp_type in interface_sym.info.types
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
							// matches_interface = '/*iface:$got_type $exp_type*/ true'
							//}
						}
					} else if left is ast.SelectorExpr {
						if left.gkind_field == .typ {
							exp_type = g.unwrap_generic(left.name_type)
						} else {
							name = '${left.expr}.${left.field_name}'
							exp_type = g.comptime_var_type_map[name]
						}
					} else if left is ast.TypeNode {
						// this is only allowed for generics currently, otherwise blocked by checker
						exp_type = g.unwrap_generic(left.typ)
					}

					if cond.op == .key_is {
						g.write('${exp_type.idx()} == ${got_type.idx()} && ${exp_type.has_flag(.optional)} == ${got_type.has_flag(.optional)}')
						return exp_type == got_type, true
					} else {
						g.write('${exp_type.idx()} != ${got_type.idx()}')
						return exp_type != got_type, true
					}
				}
				.eq, .ne {
					// TODO Implement `$if method.args.len == 1`
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
			if g.inside_comptime_for_field && cond.expr is ast.Ident
				&& (cond.expr as ast.Ident).name == g.comptime_for_field_var && cond.field_name in ['is_mut', 'is_pub', 'is_shared', 'is_atomic', 'is_optional', 'is_array', 'is_map', 'is_chan', 'is_struct', 'is_alias'] {
				ret_bool := g.get_comptime_selector_bool_field(cond.field_name)
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

//

struct CurrentComptimeValues {
	inside_comptime_for_field bool
	comptime_for_method       string
	comptime_for_field_var    string
	comptime_for_field_value  ast.StructField
	comptime_for_field_type   ast.Type
	comptime_var_type_map     map[string]ast.Type
}

fn (mut g Gen) push_existing_comptime_values() {
	g.comptime_values_stack << CurrentComptimeValues{g.inside_comptime_for_field, g.comptime_for_method, g.comptime_for_field_var, g.comptime_for_field_value, g.comptime_for_field_type, g.comptime_var_type_map.clone()}
}

fn (mut g Gen) pop_existing_comptime_values() {
	old := g.comptime_values_stack.pop()
	g.inside_comptime_for_field = old.inside_comptime_for_field
	g.comptime_for_method = old.comptime_for_method
	g.comptime_for_field_var = old.comptime_for_field_var
	g.comptime_for_field_value = old.comptime_for_field_value
	g.comptime_for_field_type = old.comptime_for_field_type
	g.comptime_var_type_map = old.comptime_var_type_map.clone()
}

//

fn (mut g Gen) comptime_for(node ast.ComptimeFor) {
	sym := g.table.final_sym(g.unwrap_generic(node.typ))
	g.writeln('/* \$for ${node.val_var} in ${sym.name}(${node.kind.str()}) */ {')
	g.indent++
	// vweb_result_type := ast.new_type(g.table.find_type_idx('vweb.Result'))
	mut i := 0
	// g.writeln('string method = _SLIT("");')
	if node.kind == .methods {
		mut methods := sym.methods.filter(it.attrs.len == 0) // methods without attrs first
		methods_with_attrs := sym.methods.filter(it.attrs.len > 0) // methods with attrs second
		methods << methods_with_attrs
		if methods.len > 0 {
			g.writeln('FunctionData ${node.val_var} = {0};')
		}
		typ_vweb_result := g.table.find_type_idx('vweb.Result')
		for method in methods {
			g.push_existing_comptime_values()
			// filter vweb route methods (non-generic method)
			if method.receiver_type != 0 && method.return_type == typ_vweb_result {
				rec_sym := g.table.sym(method.receiver_type)
				if rec_sym.kind == .struct_ {
					if _ := g.table.find_field_with_embeds(rec_sym, 'Context') {
						if method.generic_names.len > 0
							|| (method.params.len > 1 && method.attrs.len == 0) {
							g.pop_existing_comptime_values()
							continue
						}
					}
				}
			}
			g.comptime_for_method = method.name
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
					g.comptime_var_type_map['${node.val_var}.args[${j}].typ'] = typ
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
			ret_typ := method.return_type.idx()
			g.writeln('\t${node.val_var}.typ = ${styp};')
			g.writeln('\t${node.val_var}.return_type = ${ret_typ};')
			//
			g.comptime_var_type_map['${node.val_var}.return_type'] = ret_typ
			g.comptime_var_type_map['${node.val_var}.typ'] = styp
			g.stmts(node.stmts)
			i++
			g.writeln('}')
			g.pop_existing_comptime_values()
		}
	} else if node.kind == .fields {
		// TODO add fields
		if sym.kind == .struct_ {
			sym_info := sym.info as ast.Struct
			if sym_info.fields.len > 0 {
				g.writeln('\tFieldData ${node.val_var} = {0};')
			}
			for field in sym_info.fields {
				g.push_existing_comptime_values()
				g.inside_comptime_for_field = true
				g.comptime_for_field_var = node.val_var
				g.comptime_for_field_value = field
				g.comptime_for_field_type = field.typ
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
				// g.writeln('\t${node.val_var}.typ = _SLIT("$field_sym.name");')
				styp := field.typ
				unaliased_styp := g.table.unaliased_type(styp)

				g.writeln('\t${node.val_var}.typ = ${styp.idx()};')
				g.writeln('\t${node.val_var}.unaliased_typ = ${unaliased_styp.idx()};')
				g.writeln('\t${node.val_var}.is_pub = ${field.is_pub};')
				g.writeln('\t${node.val_var}.is_mut = ${field.is_mut};')
				//
				g.writeln('\t${node.val_var}.is_shared = ${field.typ.has_flag(.shared_f)};')
				g.writeln('\t${node.val_var}.is_atomic = ${field.typ.has_flag(.atomic_f)};')
				g.writeln('\t${node.val_var}.is_optional = ${field.typ.has_flag(.optional)};')
				//
				g.writeln('\t${node.val_var}.is_array = ${field_sym.kind in [.array, .array_fixed]};')
				g.writeln('\t${node.val_var}.is_map = ${field_sym.kind == .map};')
				g.writeln('\t${node.val_var}.is_chan = ${field_sym.kind == .chan};')
				g.writeln('\t${node.val_var}.is_struct = ${field_sym.kind == .struct_};')
				g.writeln('\t${node.val_var}.is_alias = ${field_sym.kind == .alias};')
				//
				g.writeln('\t${node.val_var}.indirections = ${field.typ.nr_muls()};')
				//
				g.comptime_var_type_map['${node.val_var}.typ'] = styp
				g.stmts(node.stmts)
				i++
				g.writeln('}')
				g.pop_existing_comptime_values()
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
			}
		}
	}
	g.indent--
	g.writeln('}// \$for')
}

fn (mut g Gen) comptime_if_to_ifdef(name string, is_comptime_optional bool) ?string {
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
		else {
			if is_comptime_optional
				|| (g.pref.compile_defines_all.len > 0 && name in g.pref.compile_defines_all) {
				return 'CUSTOM_DEFINE_${name}'
			}
			return error('bad os ifdef name "${name}"') // should never happen, caught in the checker
		}
	}
	return none
}
