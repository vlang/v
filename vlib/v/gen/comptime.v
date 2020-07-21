// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.ast
import v.table
import v.util

fn (g &Gen) comptime_call(node ast.ComptimeCall) {
	if node.is_vweb {
		for stmt in node.vweb_tmpl.stmts {
			if stmt is ast.FnDecl {
				// insert stmts from vweb_tmpl fn
				if stmt.name.starts_with('main.vweb_tmpl') {
					g.inside_vweb_tmpl = true
					g.stmts(stmt.stmts)
					g.inside_vweb_tmpl = false
					break
				}
			}
		}
		g.writeln('vweb__Context_html(&app->vweb, _tmpl_res_$g.fn_decl.name); strings__Builder_free(&sb); string_free(&_tmpl_res_$g.fn_decl.name);')
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
		if m.args.len > 1 {
			g.write(', ')
		}
		for i in 1 .. m.args.len{
			if m.args[i].name == 'app' {
				continue
			}
			if int(m.args[i].typ) == table.string_type_idx {
				g.write('((string*)${node.args_var}.data) [${i-1}] ')
			} else {
				type_name := g.table.types[int(m.args[i].typ)].str()
				g.write('string_${type_name}(((string*)${node.args_var}.data) [${i-1}])')
			}
			if i < m.args.len - 1 {
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
		if method.args.len != 1 {
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

fn (mut g Gen) comp_if(it ast.CompIf) {
	if it.stmts.len == 0 && it.else_stmts.len == 0 {
		return
	}
	ifdef := g.comp_if_to_ifdef(it.val, it.is_opt)
	g.empty_line = false
	if it.is_not {
		g.writeln('// \$if !$it.val {\n#ifndef ' + ifdef)
	} else {
		g.writeln('// \$if  $it.val {\n#ifdef ' + ifdef)
	}
	// NOTE: g.defer_ifdef is needed for defers called witin an ifdef
	// in v1 this code would be completely excluded
	g.defer_ifdef = if it.is_not { '#ifndef ' + ifdef } else { '#ifdef ' + ifdef }
	// println('comp if stmts $g.file.path:$it.pos.line_nr')
	g.indent--
	g.stmts(it.stmts)
	g.indent++
	g.defer_ifdef = ''
	if it.has_else {
		g.empty_line = false
		g.writeln('#else')
		g.defer_ifdef = if it.is_not { '#ifdef ' + ifdef } else { '#ifndef ' + ifdef }
		g.indent--
		g.stmts(it.else_stmts)
		g.indent++
		g.defer_ifdef = ''
	}
	g.empty_line = false
	g.writeln('#endif\n// } $it.val')
}

fn (mut g Gen) comp_for(node ast.CompFor) {
	g.writeln('// 2comptime $' + 'for {')
	sym := g.table.get_type_symbol(g.unwrap_generic(node.typ))
	vweb_result_type := table.new_type(g.table.find_type_idx('vweb.Result'))
	mut i := 0
	// g.writeln('string method = tos_lit("");')
	mut methods := sym.methods.filter(it.attrs.len == 0) // methods without attrs first
	methods_with_attrs := sym.methods.filter(it.attrs.len > 0) // methods without attrs first
	methods << methods_with_attrs
	for method in methods { // sym.methods {
		// if method.attrs.len == 0 {
		// continue
		// }
		if method.return_type != vweb_result_type { // table.void_type {
			continue
		}
		g.comp_for_method = method.name
		g.writeln('\t// method $i')
		if i == 0 {
			g.write('\tstring ')
		}
		g.writeln('method = tos_lit("$method.name");')
		if i == 0 {
			g.write('\tarray_string ')
		}
		if method.attrs.len == 0 {
			g.writeln('attrs = new_array_from_c_array(0, 0, sizeof(string), _MOV((string[0]){}));')
		} else {
			mut attrs := []string{}
			for attrib in method.attrs {
				attrs << 'tos_lit("$attrib")'
			}
			g.writeln('attrs = new_array_from_c_array($attrs.len, $attrs.len, sizeof(string), _MOV((string[$attrs.len]){' + attrs.join(', ') + '}));')
		}
		g.stmts(node.stmts)
		i++
		g.writeln('')
	}
	g.writeln('// } comptime for')
}
