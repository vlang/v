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
	mut name := node.cname
	mut expr_type := node.expr_type

	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 {
		// generic func with recursion rewrite node.expr_type
		if node.expr is ast.Ident {
			// var
			if node.expr.info is ast.IdentVar && node.expr.language == .v {
				name = g.typ(g.unwrap_generic(node.expr.info.typ.clear_flags(.shared_f,
					.result))).replace('*', '')
			}
		}
	}
	// var.${field.name}
	if node.expr is ast.ComptimeSelector {
		if node.expr.field_expr is ast.SelectorExpr {
			if node.expr.field_expr.expr is ast.Ident {
				if node.expr.field_expr.expr.name == g.comptime_for_field_var
					&& node.expr.field_expr.field_name == 'name' {
					field, _ := g.get_comptime_selector_var_type(node.expr)
					name = g.typ(g.unwrap_generic(field.typ.clear_flags(.shared_f, .result)))
					expr_type = field.typ
				}
			}
		}
	} else if node.expr is ast.Ident && g.inside_comptime_for_field && g.is_comptime_var(node.expr) {
		expr_type = g.get_comptime_var_type(node.expr)
		name = g.typ(g.unwrap_generic(expr_type.clear_flags(.shared_f, .result))).replace('*',
			'')
	}

	if g.table.sym(node.expr_type).language == .c {
		name = name[3..]
	}
	dump_fn_name := '_v_dump_expr_${name}' +
		(if expr_type.is_ptr() { '_ptr'.repeat(expr_type.nr_muls()) } else { '' })
	g.write(' ${dump_fn_name}(${ctoslit(fpath)}, ${line}, ${sexpr}, ')
	if expr_type.has_flag(.shared_f) {
		g.write('&')
		g.expr(node.expr)
		g.write('->val')
	} else if expr_type.has_flag(.result) {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		g.write('(*(${name}*)')
		g.expr(node.expr)
		g.write('.data)')
		g.inside_opt_or_res = old_inside_opt_or_res
	} else {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		g.expr(node.expr)
		g.inside_opt_or_res = old_inside_opt_or_res
	}
	g.write(')')
}

fn (mut g Gen) dump_expr_definitions() {
	mut dump_typedefs := map[string]bool{}
	mut dump_fns := strings.new_builder(100)
	mut dump_fn_defs := strings.new_builder(100)
	for dump_type, cname in g.table.dumps {
		dump_sym := g.table.sym(dump_type)
		mut name := cname
		if dump_sym.language == .c {
			name = name[3..]
		}
		_, str_method_expects_ptr, _ := dump_sym.str_method_info()
		typ := ast.Type(dump_type)
		is_ptr := typ.is_ptr()
		deref, _ := deref_kind(str_method_expects_ptr, is_ptr, dump_type)
		to_string_fn_name := g.get_str_fn(typ.clear_flags(.shared_f, .result))
		mut ptr_asterisk := if is_ptr { '*'.repeat(typ.nr_muls()) } else { '' }
		mut str_dumparg_type := ''
		mut str_dumparg_ret_type := ''
		if dump_sym.kind == .none_ {
			str_dumparg_type = 'IError' + ptr_asterisk
		} else {
			if typ.has_flag(.option) {
				str_dumparg_type += '_option_'
				ptr_asterisk = ptr_asterisk.replace('*', '_ptr')
			}
			str_dumparg_type += g.cc_type(dump_type, true) + ptr_asterisk
		}
		if dump_sym.kind == .function {
			fninfo := dump_sym.info as ast.FnType
			str_dumparg_type = 'DumpFNType_${name}'
			tdef_pos := g.out.len
			g.write_fn_ptr_decl(&fninfo, str_dumparg_type)
			str_tdef := g.out.after(tdef_pos)
			g.go_back(str_tdef.len)
			dump_typedefs['typedef ${str_tdef};'] = true
			str_dumparg_ret_type = str_dumparg_type
		} else if !typ.has_flag(.option) && dump_sym.kind == .array_fixed {
			// fixed array returned from function
			str_dumparg_ret_type = '_v_' + str_dumparg_type
		} else {
			str_dumparg_ret_type = str_dumparg_type
		}
		dump_fn_name := '_v_dump_expr_${name}' +
			(if is_ptr { '_ptr'.repeat(typ.nr_muls()) } else { '' })
		dump_fn_defs.writeln('${str_dumparg_ret_type} ${dump_fn_name}(string fpath, int line, string sexpr, ${str_dumparg_type} dump_arg);')
		if g.writeln_fn_header('${str_dumparg_ret_type} ${dump_fn_name}(string fpath, int line, string sexpr, ${str_dumparg_type} dump_arg)', mut
			dump_fns)
		{
			continue
		}
		mut surrounder := util.new_surrounder(3)
		surrounder.add('\tstring sline = int_str(line);', '\tstring_free(&sline);')
		if dump_sym.kind == .function {
			surrounder.add('\tstring value = ${to_string_fn_name}();', '\tstring_free(&value);')
		} else if dump_sym.kind == .none_ {
			surrounder.add('\tstring value = _SLIT("none");', '\tstring_free(&value);')
		} else if is_ptr {
			if typ.has_flag(.option) {
				surrounder.add('\tstring value = isnil(&dump_arg.data) ? _SLIT("nil") : ${to_string_fn_name}(${deref}dump_arg);',
					'\tstring_free(&value);')
			} else {
				surrounder.add('\tstring value = (dump_arg == NULL) ? _SLIT("nil") : ${to_string_fn_name}(${deref}dump_arg);',
					'\tstring_free(&value);')
			}
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
			for i := 0; i < typ.nr_muls(); i++ {
				dump_fns.writeln("\tstrings__Builder_write_rune(&sb, '&');")
			}
		}
		dump_fns.writeln('\tstrings__Builder_write_string(&sb, value);')
		dump_fns.writeln("\tstrings__Builder_write_rune(&sb, '\\n');")
		surrounder.builder_write_afters(mut dump_fns)
		if !typ.has_flag(.option) && dump_sym.kind == .array_fixed {
			tmp_var := g.new_tmp_var()
			dump_fns.writeln('\t${str_dumparg_ret_type} ${tmp_var} = {0};')
			dump_fns.writeln('\tmemcpy(${tmp_var}.ret_arr, dump_arg, sizeof(${str_dumparg_type}));')
			dump_fns.writeln('\treturn ${tmp_var};')
		} else {
			dump_fns.writeln('\treturn dump_arg;')
		}
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
		sb.writeln('${s};')
		return true
	}
	sb.writeln('${s} {')
	return false
}
