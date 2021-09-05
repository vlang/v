module js

import v.ast

fn (mut g JsGen) gen_method_call(it ast.CallExpr) bool {
	g.call_stack << it

	mut name := g.js_name(it.name)
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return builtin.unwrap(')
	}
	sym := g.table.get_type_symbol(it.receiver_type)
	if sym.kind == .array {
		if sym.kind == .array && it.name in ['map', 'filter'] {
			g.expr(it.left)
			mut ltyp := it.left_type
			for ltyp.is_ptr() {
				g.write('.val')
				ltyp = ltyp.deref()
			}
			g.write('.')
			// Prevent 'it' from getting shadowed inside the match
			node := it
			g.write(it.name)
			g.write('(')
			expr := node.args[0].expr
			match expr {
				ast.AnonFn {
					g.gen_fn_decl(expr.decl)
					g.write(')')
					return true
				}
				ast.Ident {
					if expr.kind == .function {
						g.write(g.js_name(expr.name))
						g.write(')')
						return true
					} else if expr.kind == .variable {
						v_sym := g.table.get_type_symbol(expr.var_info().typ)
						if v_sym.kind == .function {
							g.write(g.js_name(expr.name))
							g.write(')')
							return true
						}
					}
				}
				else {}
			}

			g.write('it => ')
			g.expr(node.args[0].expr)
			g.write(')')
			return true
		}

		left_sym := g.table.get_type_symbol(it.left_type)
		if left_sym.kind == .array {
			if it.name in special_array_methods {
				g.expr(it.left)
				mut ltyp := it.left_type
				for ltyp.is_ptr() {
					g.write('.val')
					ltyp = ltyp.deref()
				}
				g.write('.')

				g.gen_array_method_call(it)
				return true
			}
		}
	}

	mut ltyp := it.left_type
	mut lsym := g.table.get_type_symbol(ltyp)
	if lsym.kind == .interface_ {
		g.write(g.js_name(lsym.name))
		g.write('.${name}.call(')
		g.expr(it.left)
		g.write(',')
		for i, arg in it.args {
			g.expr(arg.expr)
			if i != it.args.len - 1 {
				g.write(', ')
			}
		}
		// end method call
		g.write(')')
	} else {
		g.write('Object.getPrototypeOf(')
		g.expr(it.left)

		for ltyp.is_ptr() {
			g.write('.val')
			ltyp = ltyp.deref()
		}
		g.write(').$name .call(')
		g.expr(it.left)
		g.write(',')
		for i, arg in it.args {
			g.expr(arg.expr)
			if i != it.args.len - 1 {
				g.write(', ')
			}
		}
		// end method call
		g.write(')')
	}

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
					g.writeln('return builtin.panic($panicstr)')
				} else {
					g.writeln('builtin.js_throw(err)')
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
	return true
}

fn (mut g JsGen) gen_call_expr(it ast.CallExpr) {
	if it.is_method {
		if g.gen_method_call(it) {
			return
		}
	}
	node := it
	g.call_stack << it
	mut name := g.js_name(it.name)
	is_print := name in ['print', 'println', 'eprint', 'eprintln', 'panic']
	print_method := name
	ret_sym := g.table.get_type_symbol(it.return_type)
	if it.language == .js && ret_sym.name in v_types && ret_sym.name != 'void' {
		g.write('new ')
		if g.ns.name != 'builtin' {
			g.write('builtin.')
		}
		g.write(ret_sym.name)
		g.write('(')
	}
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return builtin.unwrap(')
	}
	if is_print {
		mut typ := node.args[0].typ

		expr := node.args[0].expr
		g.write('builtin.$print_method (')
		g.gen_expr_to_string(expr, typ)
		g.write(')')
		return
	}
	g.expr(it.left)

	if name in g.builtin_fns {
		g.write('builtin.')
	}

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
				panicstr := '`optional not set (\${err})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return builtin.panic($panicstr)')
				} else {
					g.writeln('builtin.js_throw(err)')
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
