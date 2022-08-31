module c

import v.ast
import v.util
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
	if node.expr_type.has_flag(.shared_f) {
		g.write('&')
		g.expr(node.expr)
		g.write('->val')
	} else {
		g.expr(node.expr)
	}
	g.write(' )')
}

fn (mut g Gen) dump_expr_definitions() {
	mut dump_typedefs := map[string]bool{}
	mut dump_fns := strings.new_builder(100)
	mut dump_fn_defs := strings.new_builder(100)
	for dump_type, cname in g.table.dumps {
		dump_sym := g.table.sym(dump_type)
		_, str_method_expects_ptr, _ := dump_sym.str_method_info()
		is_ptr := ast.Type(dump_type).is_ptr()
		deref, _ := deref_kind(str_method_expects_ptr, is_ptr, dump_type)
		to_string_fn_name := g.get_str_fn(ast.Type(dump_type).clear_flag(.shared_f))
		ptr_asterisk := if is_ptr { '*' } else { '' }
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
		dump_fn_defs.writeln('$str_dumparg_type ${dump_fn_name}(string fpath, int line, string sexpr, $str_dumparg_type dump_arg);')
		if g.writeln_fn_header('$str_dumparg_type ${dump_fn_name}(string fpath, int line, string sexpr, $str_dumparg_type dump_arg)', mut
			dump_fns)
		{
			continue
		}
		mut surrounder := util.new_surrounder(3)
		surrounder.add('\tstring sline = int_str(line);', '\tstring_free(&sline);')
		if dump_sym.kind == .function {
			surrounder.add('\tstring value = ${to_string_fn_name}();', '\tstring_free(&value);')
		} else {
			surrounder.add('\tstring value = ${to_string_fn_name}(${deref}dump_arg);',
				'\tstring_free(&value);')
		}
		surrounder.add('
	strings__Builder sb = strings__new_builder(256);
', '
	string res;
	res = strings__Builder_str(&sb);
	eprint(res);
	string_free(&res);
	strings__Builder_free(&sb);
')
		surrounder.builder_write_befores(mut dump_fns)
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, '[');")
		dump_fns.writeln('\tstrings__Builder_write_string(&sb, fpath);')
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, ':');")
		dump_fns.writeln('\tstrings__Builder_write_string(&sb, sline);')
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, ']');")
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, ' ');")
		dump_fns.writeln('\tstrings__Builder_write_string(&sb, sexpr);')
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, ':');")
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, ' ');")
		if is_ptr {
			dump_fns.writeln("\tstrings__Builder_write_rune(&sb, '&');")
		}
		dump_fns.writeln('\tstrings__Builder_write_string(&sb, value);')
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, '\\n');")
		surrounder.builder_write_afters(mut dump_fns)
		dump_fns.writeln('\treturn dump_arg;')
		dump_fns.writeln('}')
	}
	for tdef, _ in dump_typedefs {
		g.definitions.writeln(tdef)
	}
	g.definitions.writeln(dump_fn_defs.str())
	g.dump_funcs.writeln(dump_fns.str())
}

fn (mut g Gen) writeln_fn_header(s string, mut sb strings.Builder) bool {
	if g.pref.build_mode == .build_module {
		sb.writeln('$s;')
		return true
	}
	sb.writeln('$s {')
	return false
}
