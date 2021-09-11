module js

import v.ast
import v.util

fn (mut g JsGen) js_mname(name_ string) string {
	mut is_js := false
	is_overload := ['+', '-', '*', '/', '==', '<', '>']
	mut name := name_
	if name.starts_with('JS.') {
		name = name[3..]
		is_js = true
	}
	ns := get_ns(name)
	name = if name in is_overload {
		match name {
			'+' {
				'\$add'
			}
			'-' {
				'\$sub'
			}
			'/' {
				'\$div'
			}
			'*' {
				'\$mul'
			}
			'%' {
				'\$mod'
			}
			'==' {
				'eq'
			}
			'>' {
				'\$gt'
			}
			'<' {
				'\$lt'
			}
			else {
				''
			}
		}
	} else if g.ns == 0 {
		name
	} else if ns == g.ns.name {
		name.split('.').last()
	} else {
		g.get_alias(name)
	}
	mut parts := name.split('.')
	if !is_js {
		for i, p in parts {
			if p in js_reserved {
				parts[i] = 'v_$p'
			}
		}
	}
	return parts.join('.')
}

fn (mut g JsGen) js_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	g.write('${g.js_mname(it.name)}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end call
	g.write(')')
	g.call_stack.delete_last()
}

fn (mut g JsGen) js_method_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	g.expr(it.left)
	g.write('.${g.js_mname(it.name)}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end method call
	g.write(')')
	g.call_stack.delete_last()
}

fn (mut g JsGen) method_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return builtin.unwrap(')
	}
	mut unwrapped_rec_type := node.receiver_type
	if g.table.cur_fn.generic_names.len > 0 {
		unwrapped_rec_type = g.unwrap_generic(node.receiver_type)
	} else {
		sym := g.table.get_type_symbol(node.receiver_type)
		match sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				generic_names := sym.info.generic_types.map(g.table.get_type_symbol(it).name)
				if utyp := g.table.resolve_generic_to_concrete(node.receiver_type, generic_names,
					sym.info.concrete_types)
				{
					unwrapped_rec_type = utyp
				}
			}
			else {}
		}
	}

	mut typ_sym := g.table.get_type_symbol(unwrapped_rec_type)
	rec_cc_type := g.cc_type(unwrapped_rec_type, false)
	mut receiver_type_name := util.no_dots(rec_cc_type)
	// alias type that undefined this method (not include `str`) need to use parent type
	if typ_sym.kind == .alias && node.name != 'str' && !typ_sym.has_method(node.name) {
		unwrapped_rec_type = (typ_sym.info as ast.Alias).parent_type
		typ_sym = g.table.get_type_symbol(unwrapped_rec_type)
	}

	if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method(node.name) {
		g.expr(it.left)
		g.gen_deref_ptr(it.left_type)
		g.write('.${it.name}(')
		for i, arg in it.args {
			g.expr(arg.expr)
			if i != it.args.len - 1 {
				g.write(', ')
			}
		}
		g.write(')')
		return
	}

	left_sym := g.table.get_type_symbol(node.left_type)
	final_left_sym := g.table.get_final_type_symbol(node.left_type)

	if final_left_sym.kind == .array {
		if final_left_sym.kind == .array && it.name in ['map', 'filter'] {
			g.expr(it.left)
			mut ltyp := it.left_type
			for ltyp.is_ptr() {
				g.write('.val')
				ltyp = ltyp.deref()
			}
			g.write('.')
			// Prevent 'it' from getting shadowed inside the match

			g.write(it.name)
			g.write('(')
			expr := node.args[0].expr
			match expr {
				ast.AnonFn {
					g.gen_fn_decl(expr.decl)
					g.write(')')
					return
				}
				ast.Ident {
					if expr.kind == .function {
						g.write(g.js_name(expr.name))
						g.write(')')
						return
					} else if expr.kind == .variable {
						v_sym := g.table.get_type_symbol(expr.var_info().typ)
						if v_sym.kind == .function {
							g.write(g.js_name(expr.name))
							g.write(')')
							return
						}
					}
				}
				else {}
			}

			g.write('it => ')
			g.expr(node.args[0].expr)
			g.write(')')
			return
		}

		if final_left_sym.kind == .array {
			if it.name in special_array_methods {
				g.gen_array_method_call(it)
				return
			}
		}
	}
	if final_left_sym.kind == .array
		&& node.name in ['repeat', 'sort_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice', 'pointers'] {
		if !(left_sym.info is ast.Alias && typ_sym.has_method(node.name)) {
			// `array_Xyz_clone` => `array_clone`
			receiver_type_name = 'array'
		}
	}
	mut name := util.no_dots('${receiver_type_name}_$node.name')

	name = g.generic_fn_name(node.concrete_types, name, false)
	g.write('${name}(')
	g.expr(it.left)
	g.write(',')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')

	if call_return_is_optional {
		// end unwrap
		g.writeln(')')
		g.dec_indent()
		// begin catch block
		g.writeln('} catch(err) {')
		g.inc_indent()
		// gen or block contents
		match it.or_block.kind {
			.block {
				if it.or_block.stmts.len > 1 {
					g.stmts(it.or_block.stmts[..it.or_block.stmts.len - 1])
				}
				// g.write('return ')
				g.stmt(it.or_block.stmts.last())
			}
			.propagate {
				panicstr := '`optional not set (\${err})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return panic($panicstr)')
				} else {
					g.writeln('js_throw(err)')
				}
			}
			else {}
		}
		// end catch
		g.dec_indent()
		g.writeln('}')
		// end anon fn
		g.dec_indent()
		g.write('})()')
	}
	g.call_stack.delete_last()
}

fn (mut g JsGen) gen_call_expr(it ast.CallExpr) {
	if it.is_method && g.table.get_type_symbol(it.receiver_type).name.starts_with('JS.') {
		g.js_method_call(it)
		return
	} else if it.name.starts_with('JS.') {
		g.js_call(it)
		return
	}
	if it.is_method {
		g.method_call(it)
		return
	}
	node := it
	g.call_stack << it
	mut name := g.js_name(it.name)
	is_print := name in ['print', 'println', 'eprint', 'eprintln', 'panic']
	print_method := name
	ret_sym := g.table.get_type_symbol(it.return_type)
	if it.language == .js && ret_sym.name in v_types && ret_sym.name != 'void' {
		g.write('new ')
		g.write(ret_sym.name)
		g.write('(')
	}
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return unwrap(')
	}
	if is_print {
		mut typ := node.args[0].typ

		expr := node.args[0].expr
		g.write('$print_method (')
		g.gen_expr_to_string(expr, typ)
		g.write(')')
		return
	}
	g.expr(it.left)

	g.write('${name}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end method call
	g.write(')')
	if call_return_is_optional {
		// end unwrap
		g.writeln(')')
		g.dec_indent()
		// begin catch block
		g.writeln('} catch(err) {')
		g.inc_indent()
		// gen or block contents
		match it.or_block.kind {
			.block {
				if it.or_block.stmts.len > 1 {
					g.stmts(it.or_block.stmts[..it.or_block.stmts.len - 1])
				}

				//	g.write('return ')
				g.stmt(it.or_block.stmts.last())
			}
			.propagate {
				panicstr := '`optional not set (\${err.val.msg})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return panic($panicstr)')
				} else {
					g.writeln('js_throw(err)')
				}
			}
			else {}
		}
		// end catch
		g.dec_indent()
		g.writeln('}')
		// end anon fn
		g.dec_indent()
		g.write('})()')
	}
	if it.language == .js && ret_sym.name in v_types && ret_sym.name != 'void' {
		g.write(')')
	}
	g.call_stack.delete_last()
}
