module c

import v.ast
import strings

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
	mut dump_typedefs := map[string]bool{}
	mut dump_fns := strings.new_builder(100)
	for dump_type, cname in g.table.dumps {
		to_string_fn_name := g.get_str_fn(dump_type)
		is_ptr := ast.Type(dump_type).is_ptr()
		ptr_asterisk := if is_ptr { '*' } else { '' }
		dump_sym := g.table.get_type_symbol(dump_type)
		mut str_dumparg_type := '$cname$ptr_asterisk'
		if dump_sym.kind == .function {
			fninfo := dump_sym.info as ast.FnType
			str_dumparg_type = 'DumpFNType_$cname'
			tdef_pos := g.out.len
			g.write_fn_ptr_decl(&fninfo, str_dumparg_type)
			str_tdef := g.out.after(tdef_pos)
			g.out.go_back(str_tdef.len)
			dump_typedefs['typedef $str_tdef;'] = true
		}
		dump_fn_name := '_v_dump_expr_$cname' + (if is_ptr { '_ptr' } else { '' })
		if g.writeln_fn_header('$str_dumparg_type ${dump_fn_name}(string fpath, int line, string sexpr, $str_dumparg_type x)', mut
			dump_fns)
		{
			continue
		}
		dump_fns.writeln('\teprint(${ctoslit('[')});')
		dump_fns.writeln('\teprint(fpath);')
		dump_fns.writeln('\teprint(${ctoslit(':')});')
		dump_fns.writeln('\teprint(int_str(line));')
		dump_fns.writeln('\teprint(${ctoslit('] ')});')
		// dump_fns.writeln('\t/* dump_type: $dump_type | to_string_fn_name: $to_string_fn_name | is_ptr: $is_ptr | ptr_asterisk: $ptr_asterisk | dump_fn_name: $dump_fn_name | cnam: $cname */')
		dump_fns.writeln('\teprint(sexpr);')
		dump_fns.writeln('\teprint(${ctoslit(': ')});')
		if is_ptr {
			dump_fns.writeln('\teprint(${ctoslit('&')});')
		}
		dump_fns.writeln('\teprintln(${to_string_fn_name}(${ptr_asterisk}x));')
		dump_fns.writeln('\treturn x;')
		dump_fns.writeln('}')
	}
	for tdef, _ in dump_typedefs {
		g.definitions.writeln(tdef)
	}
	g.definitions.writeln(dump_fns.str())
}

fn (mut g Gen) writeln_fn_header(s string, mut sb strings.Builder) bool {
	if g.pref.build_mode == .build_module {
		sb.writeln('$s;')
		return true
	}
	sb.writeln('$s {')
	return false
}
