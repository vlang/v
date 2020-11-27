// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.ast
import v.table
import v.util

fn (mut g Gen) comptime_call(node ast.ComptimeCall) {
	if node.is_vweb {
		is_html := node.method_name == 'html'
		for stmt in node.vweb_tmpl.stmts {
			if stmt is ast.FnDecl {
				// insert stmts from vweb_tmpl fn
				if stmt.name.starts_with('main.vweb_tmpl') {
					if is_html {
						g.inside_vweb_tmpl = true
					}
					g.stmts(stmt.stmts)
					g.inside_vweb_tmpl = false
					break
				}
			}
		}
		if is_html {
			// return vweb html template
			g.writeln('vweb__Context_html(&app->vweb, _tmpl_res_$g.fn_decl.name); strings__Builder_free(&sb); string_free(&_tmpl_res_$g.fn_decl.name);')
		} else {
			// return $tmpl string
			fn_name := g.fn_decl.name.replace('.', '__')
			g.writeln('return _tmpl_res_$fn_name;')
		}
		return
	}
	g.writeln('// $' + 'method call. sym="$node.sym.name"')
	mut j := 0
	result_type := g.table.find_type_idx('vweb.Result') // TODO not just vweb
	if node.method_name == 'method' {
		// `app.$method()`
		m := node.sym.find_method(g.comp_for_method) or {
			return
		}
		/*
		vals := m.attrs[0].split('/')
		args := vals.filter(it.starts_with(':')).map(it[1..])
		println(vals)
		for val in vals {
		}
		*/
		g.write('${util.no_dots(node.sym.name)}_${g.comp_for_method}(')
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
			if m.params[i].typ.is_int() || m.params[i].typ.idx() == table.bool_type_idx {
				// Gets the type name and cast the string to the type with the string_<type> function
				type_name := g.table.types[int(m.params[i].typ)].str()
				g.write('string_${type_name}(((string*)${node.args_var}.data) [${i - 1}])')
			} else {
				g.write('((string*)${node.args_var}.data) [${i - 1}] ')
			}
			if i < m.params.len - 1 {
				g.write(', ')
			}
		}
		g.write(' ); // vweb action call with args')
		return
	}
	for method in node.sym.methods {
		// if method.return_type != table.void_type {
		if method.return_type != result_type {
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
		if j > 0 {
			g.write(' else ')
		}
		g.write('if (string_eq($node.method_name, tos_lit("$method.name"))) ')
		g.write('${util.no_dots(node.sym.name)}_${method.name}($amp ')
		g.expr(node.left)
		g.writeln(');')
		j++
	}
}

fn cgen_attrs(attrs []table.Attr) []string {
	mut res := []string{cap: attrs.len}
	for attr in attrs {
		// we currently don't quote 'arg' (otherwise we could just use `s := attr.str()`)
		mut s := attr.name
		if attr.arg.len > 0 {
			s += ': $attr.arg'
		}
		res << 'tos_lit("$s")'
	}
	return res
}

fn (mut g Gen) comp_at(node ast.AtExpr) {
	if node.kind == .vmod_file {
		val := cnewlines(node.val.replace('\r', '')).replace('\\', '\\\\')
		g.write('tos_lit("$val")')
	} else {
		val := node.val.replace('\\', '\\\\')
		g.write('tos_lit("$val")')
	}
}

fn (mut g Gen) comp_if(node ast.IfExpr) {
	line := if node.is_expr {
		stmt_str := g.go_before_stmt(0)
		g.write(tabs[g.indent])
		stmt_str.trim_space()
	} else {
		''
	}
	for i, branch in node.branches {
		start_pos := g.out.len
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('#else')
		} else {
			if i == 0 {
				g.write('#if ')
			} else {
				g.write('#elif ')
			}
			g.comp_if_expr(branch.cond)
			g.writeln('')
		}
		expr_str := g.out.last_n(g.out.len - start_pos).trim_space()
		g.defer_ifdef = expr_str
		if node.is_expr {
			len := branch.stmts.len
			if len > 0 {
				last := branch.stmts[len - 1] as ast.ExprStmt
				if len > 1 {
					tmp := g.new_tmp_var()
					styp := g.typ(last.typ)
					g.indent++
					g.writeln('$styp $tmp;')
					g.writeln('{')
					g.stmts(branch.stmts[0..len - 1])
					g.write('\t$tmp = ')
					g.stmt(last)
					g.writeln('}')
					g.indent--
					g.writeln('$line $tmp;')
				} else {
					g.write('$line ')
					g.stmt(last)
				}
			}
		} else {
			// Only wrap the contents in {} if we're inside a function, not on the top level scope
			should_create_scope := g.fn_decl != 0
			if should_create_scope {
				g.writeln('{')
			}
			g.stmts(branch.stmts)
			if should_create_scope {
				g.writeln('}')
			}
		}
		g.defer_ifdef = ''
	}
	if node.is_expr {
		g.write('#endif')
	} else {
		g.writeln('#endif')
	}
}

fn (mut g Gen) comp_if_expr(cond ast.Expr) {
	match cond {
		ast.ParExpr {
			g.write('(')
			g.comp_if_expr(cond.expr)
			g.write(')')
		}
		ast.PrefixExpr {
			g.write(cond.op.str())
			g.comp_if_expr(cond.right)
		}
		ast.PostfixExpr {
			ifdef := g.comp_if_to_ifdef((cond.expr as ast.Ident).name, true)
			g.write('defined($ifdef)')
		}
		ast.InfixExpr {
			match cond.op {
				.and, .logical_or {
					g.comp_if_expr(cond.left)
					g.write(' $cond.op ')
					g.comp_if_expr(cond.right)
				}
				.key_is, .not_is {
					se := cond.left as ast.SelectorExpr
					name := '${se.expr}.$se.field_name'
					exp_type := g.comptime_var_type_map[name]
					got_type := (cond.right as ast.Type).typ
					g.write('$exp_type == $got_type')
				}
				.eq, .ne {
					// TODO Implement `$if method.args.len == 1`
				}
				else {}
			}
		}
		ast.Ident {
			ifdef := g.comp_if_to_ifdef(cond.name, false)
			g.write('defined($ifdef)')
		}
		else {}
	}
}

fn (mut g Gen) comp_for(node ast.CompFor) {
	sym := g.table.get_type_symbol(g.unwrap_generic(node.typ))
	g.writeln('{ // 2comptime: \$for $node.val_var in ${sym.name}($node.kind.str()) {')
	// vweb_result_type := table.new_type(g.table.find_type_idx('vweb.Result'))
	mut i := 0
	// g.writeln('string method = tos_lit("");')
	if node.kind == .methods {
		mut methods := sym.methods.filter(it.attrs.len == 0) // methods without attrs first
		methods_with_attrs := sym.methods.filter(it.attrs.len > 0) // methods with attrs second
		methods << methods_with_attrs
		if methods.len > 0 {
			g.writeln('\tFunctionData $node.val_var;')
			g.writeln('\tmemset(&$node.val_var, 0, sizeof(FunctionData));')
		}
		for method in methods { // sym.methods {
			/*
			if method.return_type != vweb_result_type { // table.void_type {
				continue
			}
			*/
			g.comp_for_method = method.name
			g.writeln('\t// method $i')
			g.writeln('\t${node.val_var}.name = tos_lit("$method.name");')
			if method.attrs.len == 0 {
				g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
			} else {
				attrs := cgen_attrs(method.attrs)
				g.writeln('\t${node.val_var}.attrs = new_array_from_c_array($attrs.len, $attrs.len, sizeof(string), _MOV((string[$attrs.len]){' +
					attrs.join(', ') + '}));')
			}
			if method.params.len < 2 {
				// 0 or 1 (the receiver) args
				g.writeln('\t${node.val_var}.args = __new_array_with_default(0, 0, sizeof(MethodArgs), 0);')
			} else {
				len := method.params.len - 1
				g.write('\t${node.val_var}.args = new_array_from_c_array($len, $len, sizeof(MethodArgs), _MOV((MethodArgs[$len]){')
				// Skip receiver arg
				for j, arg in method.params[1..] {
					typ := arg.typ.idx()
					g.write(typ.str())
					if j < len - 1 {
						g.write(', ')
					}
					g.comptime_var_type_map['${node.val_var}.args[$j].typ'] = typ
				}
				g.writeln('}));')
			}
			mut sig := 'anon_fn_'
			// skip the first (receiver) arg
			for j, arg in method.params[1..] {
				// TODO: ignore mut/pts in sig for now
				typ := arg.typ.set_nr_muls(0)
				sig += '$typ'
				if j < method.params.len - 2 {
					sig += '_'
				}
			}
			sig += '_$method.return_type'
			styp := g.table.find_type_idx(sig)
			// println(styp)
			// if styp == 0 { }
			// TODO: type aliases
			ret_typ := method.return_type.idx()
			g.writeln('\t${node.val_var}.typ = $styp;')
			g.writeln('\t${node.val_var}.return_type = $ret_typ;')
			//
			g.comptime_var_type_map['${node.val_var}.return_type'] = ret_typ
			g.comptime_var_type_map['${node.val_var}.typ'] = styp
			g.stmts(node.stmts)
			i++
			g.writeln('')
			for key, _ in g.comptime_var_type_map {
				if key.starts_with(node.val_var) {
					g.comptime_var_type_map.delete(key)
				}
			}
		}
	} else if node.kind == .fields {
		// TODO add fields
		if sym.info is table.Struct {
			info := sym.info as table.Struct
			mut fields := info.fields.filter(it.attrs.len == 0)
			fields_with_attrs := info.fields.filter(it.attrs.len > 0)
			fields << fields_with_attrs
			if fields.len > 0 {
				g.writeln('\tFieldData $node.val_var;')
				g.writeln('\tmemset(&$node.val_var, 0, sizeof(FieldData));')
			}
			for field in fields {
				g.writeln('\t// field $i')
				g.writeln('\t${node.val_var}.name = tos_lit("$field.name");')
				if field.attrs.len == 0 {
					g.writeln('\t${node.val_var}.attrs = __new_array_with_default(0, 0, sizeof(string), 0);')
				} else {
					attrs := cgen_attrs(field.attrs)
					g.writeln('\t${node.val_var}.attrs = new_array_from_c_array($attrs.len, $attrs.len, sizeof(string), _MOV((string[$attrs.len]){' +
						attrs.join(', ') + '}));')
				}
				// field_sym := g.table.get_type_symbol(field.typ)
				// g.writeln('\t${node.val_var}.typ = tos_lit("$field_sym.name");')
				styp := field.typ
				g.writeln('\t${node.val_var}.typ = $styp;')
				g.writeln('\t${node.val_var}.is_pub = $field.is_pub;')
				g.writeln('\t${node.val_var}.is_mut = $field.is_mut;')
				g.comptime_var_type_map['${node.val_var}.typ'] = styp
				g.stmts(node.stmts)
				i++
				g.writeln('')
			}
			g.comptime_var_type_map.delete(node.val_var)
		}
	}
	g.writeln('} // } comptime for')
}
