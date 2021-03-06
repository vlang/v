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
	g.write(' _v_dump_expr_${node.cname}(${ctoslit(fpath)}, $line, $sexpr, ')
	g.expr(node.expr)
	g.write(' )')
}

fn (mut g Gen) dump_expr_definitions() {
	if g.pref.build_mode == .build_module {
		for _, cname in g.table.dumps {
			g.definitions.writeln('$cname _v_dump_expr_${cname}(string fpath, int line, string sexpr, $cname x);')
		}
	} else {
		for dump_type, cname in g.table.dumps {
			to_string_fn_name := g.gen_str_for_type(dump_type)
			g.definitions.writeln('$cname _v_dump_expr_${cname}(string fpath, int line, string sexpr, $cname x) {')
			g.definitions.writeln('\teprint(${ctoslit('[')});')
			g.definitions.writeln('\teprint(fpath);')
			g.definitions.writeln('\teprint(${ctoslit(':')});')
			g.definitions.writeln('\teprint(int_str(line));')
			g.definitions.writeln('\teprint(${ctoslit('] ')});')
			g.definitions.writeln('\teprint(sexpr);')
			g.definitions.writeln('\teprint(${ctoslit(': ')});')
			g.definitions.writeln('\teprintln(${to_string_fn_name}(x));')
			g.definitions.writeln('\treturn x;')
			g.definitions.writeln('}')
		}
	}
}
