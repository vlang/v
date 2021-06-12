module c

import v.ast

fn (mut g Gen) dump_expr(node ast.DumpExpr) {
	sexpr := ctoslit(node.expr.str())
	fpath := cestring(g.file.path)
	line := node.pos.line_nr + 1
	if 'nop_dump' in g.pref.compile_defines {
		g.expr(node.expr)
		return
	}
	dump_fn_name := '_v_dump_expr_$node.cname' + (if node.expr_type.is_ptr() { '_ptr' } else { '' })
	g.write(' ${dump_fn_name}(${ctoslit(fpath)}, $line, $sexpr, ')
	g.expr(node.expr)
	g.write(' )')
}

fn (mut g Gen) dump_expr_definitions() {
	if g.pref.build_mode == .build_module {
		for dump_type, cname in g.table.dumps {
			is_ptr := ast.Type(dump_type).is_ptr()
			ptr_suffix := if is_ptr { '*' } else { '' }
			dump_fn_name := '_v_dump_expr_$cname' + (if is_ptr { '_ptr' } else { '' })
			g.writeln_fn_header('$cname$ptr_suffix ${dump_fn_name}(string fpath, int line, string sexpr, $cname$ptr_suffix x)')
		}
	} else {
		for dump_type, cname in g.table.dumps {
			to_string_fn_name := g.gen_str_for_type(dump_type)
			is_ptr := ast.Type(dump_type).is_ptr()
			ptr_astarisk := if is_ptr { '*' } else { '' }
			dump_fn_name := '_v_dump_expr_$cname' + (if is_ptr { '_ptr' } else { '' })
			if g.writeln_fn_header('$cname$ptr_astarisk ${dump_fn_name}(string fpath, int line, string sexpr, $cname$ptr_astarisk x)') {
				continue
			}
			g.definitions.writeln('\teprint(${ctoslit('[')});')
			g.definitions.writeln('\teprint(fpath);')
			g.definitions.writeln('\teprint(${ctoslit(':')});')
			g.definitions.writeln('\teprint(int_str(line));')
			g.definitions.writeln('\teprint(${ctoslit('] ')});')

			g.definitions.writeln('\teprint(sexpr);')
			g.definitions.writeln('\teprint(${ctoslit(': ')});')

			if is_ptr {
				g.definitions.writeln('\teprint(${ctoslit('&')});')
			}
			g.definitions.writeln('\teprintln(${to_string_fn_name}(${ptr_astarisk}x));')

			g.definitions.writeln('\treturn x;')
			g.definitions.writeln('}')
		}
	}
}

fn (mut g Gen) writeln_fn_header(s string) bool {
	if g.pref.build_mode == .build_module {
		g.definitions.writeln('$s;')
		return true
	}
	g.definitions.writeln('$s {')
	return false
}
