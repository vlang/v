module c

import v.ast
import v.util
import strings

fn (mut g Gen) dump_expr(node ast.DumpExpr) {
	sexpr := ctoslit(node.expr.str())
	fpath := cestring(g.file.path)
	line := node.pos.line_nr + 1
	if 'nop_dump' in g.pref.compile_defines {
		g.expr(ast.Expr(node.expr))
		return
	}
	mut name := node.cname
	mut expr_type := node.expr_type
	if node.expr is ast.CallExpr {
		if node.expr.return_type_generic != 0 {
			resolved_call_type := g.resolve_return_type(node.expr)
			if resolved_call_type != ast.void_type {
				expr_type = g.unwrap_generic(resolved_call_type)
				name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*',
					'')
			}
		}
	}
	if node.expr is ast.PostfixExpr {
		if node.expr.expr is ast.CallExpr && node.expr.expr.return_type_generic != 0 {
			resolved_postfix_type := g.resolve_return_type(node.expr.expr)
			if resolved_postfix_type != ast.void_type {
				expr_type = g.unwrap_generic(resolved_postfix_type)
				name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*',
					'')
			}
		}
	}
	resolved_expr_type := g.unwrap_generic(g.type_resolver.get_type_or_default(node.expr,
		expr_type))
	if resolved_expr_type != 0 && resolved_expr_type != expr_type {
		expr_type = resolved_expr_type
		name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*', '')
	}

	if node.expr is ast.CallExpr {
		g.inside_dump_fn = true
		defer(fn) {
			g.inside_dump_fn = false
		}
	}

	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 {
		// generic func with recursion rewrite node.expr_type
		if node.expr is ast.Ident {
			// var
			if node.expr.info is ast.IdentVar {
				current_fn_ident_type := g.resolve_current_fn_generic_param_type(node.expr.name)
				if current_fn_ident_type != 0 {
					expr_type = current_fn_ident_type
				} else {
					resolved_ident_type := g.unwrap_generic(g.type_resolver.get_type_or_default(ast.Expr(node.expr),
						expr_type))
					if resolved_ident_type != 0 && resolved_ident_type != expr_type {
						expr_type = resolved_ident_type
					} else {
						// For variables assigned from generic expressions
						// (e.g. `a := T{}`), scope types may be stale from a
						// previous generic instantiation. Look up the variable's
						// init expression and resolve through generic params.
						if node.expr.obj is ast.Var && node.expr.obj.expr is ast.StructInit {
							generic_names := g.current_fn_generic_names()
							short_name := node.expr.obj.expr.typ_str.all_after_last('.')
							idx := generic_names.index(short_name)
							if idx >= 0 && idx < g.cur_concrete_types.len {
								expr_type = g.cur_concrete_types[idx]
							}
						}
						if expr_type == node.expr_type {
							re := g.resolved_expr_type(node.expr, expr_type)
							if re != 0 {
								expr_type = re
							} else {
								expr_type = g.unwrap_generic(node.expr.info.typ)
							}
						}
					}
				}
				name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*',
					'')
			}
		} else if node.expr is ast.CallExpr {
			name = g.styp(g.unwrap_generic(expr_type.clear_flags(.shared_f, .result))).replace('*',
				'')
		} else {
			expr_type = g.unwrap_generic(g.type_resolver.get_type_or_default(ast.Expr(node.expr),
				expr_type))
			name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*', '')
		}
	}
	// var.$(field.name)
	if node.expr is ast.ComptimeSelector && node.expr.is_name {
		if node.expr.field_expr is ast.SelectorExpr && node.expr.field_expr.expr is ast.Ident {
			if node.expr.field_expr.expr.name == g.comptime.comptime_for_field_var {
				left_type := g.resolved_expr_type(node.expr.left, node.expr.left_type)
				field, _ := g.resolve_comptime_selector_field(node.expr, left_type)
				name = g.styp(g.unwrap_generic(field.typ.clear_flags(.shared_f, .result)))
				expr_type = field.typ
			}
		}
	} else if node.expr is ast.Ident && node.expr.ct_expr {
		for {
			if node.expr.obj is ast.Var {
				if node.expr.obj.ct_type_var == .field_var && g.comptime.inside_comptime_for
					&& g.comptime.comptime_for_field_var != '' {
					expr_type = if node.expr.obj.ct_type_unwrapped || node.expr.obj.is_unwrapped {
						g.comptime.comptime_for_field_type.clear_flag(.option)
					} else {
						g.comptime.comptime_for_field_type
					}
					break
				}
				ct_var := node.expr.obj.ct_type_var
				if ct_var == .generic_param || ct_var == .generic_var {
					if scope_var := node.expr.scope.find_var(node.expr.name) {
						if scope_var.ct_type_var == .smartcast {
							// The scope var was updated to smartcast (e.g. inside
							// `if val is v` in a comptime $for variants loop).
							// Use the comptime variant type directly.
							ctyp := g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
								scope_var.typ)
							expr_type = if scope_var.is_unwrapped {
								ctyp.clear_flag(.option)
							} else {
								ctyp
							}
							break
						} else if scope_var.ct_type_var == ct_var
							&& g.cur_fn != unsafe { nil }
							&& g.cur_fn.generic_names.len > 0 {
							// For generic_param/generic_var, node.obj.typ is a stale
							// copy from checker time. Use the refreshed scope var.
							expr_type = scope_var.typ
							break
						}
					}
				}
			}
			expr_type = g.type_resolver.get_type(ast.Expr(node.expr))
			break
		}
		name = g.styp(g.unwrap_generic(expr_type.clear_flags(.shared_f, .result))).replace('*',
			'')
	} else if node.expr is ast.SelectorExpr && node.expr.expr is ast.Ident
		&& (node.expr.expr as ast.Ident).ct_expr {
		expr_type = g.comptime_selector_type(node.expr)
		name = g.styp(g.unwrap_generic(expr_type.clear_flags(.shared_f, .result))).replace('*',
			'')
	}

	if g.table.sym(node.expr_type).language == .c {
		name = name[3..]
	}
	if node.expr is ast.Ident {
		// Don't override with the generic param type when inside a comptime
		// variant smartcast, as the variant type is more specific.
		mut is_comptime_smartcast := false
		if node.expr.ct_expr && node.expr.obj is ast.Var {
			if scope_var := node.expr.scope.find_var(node.expr.name) {
				is_comptime_smartcast = scope_var.ct_type_var == .smartcast
			}
		}
		if !is_comptime_smartcast {
			current_fn_ident_type := g.resolve_current_fn_generic_param_type(node.expr.name)
			if current_fn_ident_type != 0 {
				expr_type = current_fn_ident_type
				name = g.styp(expr_type.clear_flags(.shared_f, .result)).replace('*', '')
			}
		}
		if expr_type.is_ptr() && expr_type.has_flag(.option) {
			if scope_var := node.expr.scope.find_var(node.expr.name) {
				if scope_var.typ.has_flag(.option_mut_param_t) {
					expr_type = scope_var.typ
					name = g.styp(expr_type.clear_flags(.shared_f, .result, .option_mut_param_t)).replace('*',
						'')
				}
			}
		}
	}
	dump_fn_name := '_v_dump_expr_${name}' +
		(if expr_type.is_ptr() { '__ptr'.repeat(expr_type.nr_muls()) } else { '' })
	g.write(' ${dump_fn_name}(${ctoslit(fpath)}, ${line}, ${sexpr}, ')
	if expr_type.has_flag(.shared_f) {
		g.write('&')
		g.expr(ast.Expr(node.expr))
		g.write('->val')
	} else if expr_type.has_flag(.result) {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		g.write('(*(${name}*)')
		g.expr(ast.Expr(node.expr))
		g.write('.data)')
		g.inside_opt_or_res = old_inside_opt_or_res
	} else if node.expr is ast.ArrayInit {
		if node.expr.is_fixed {
			s := g.styp(node.expr.typ)
			if !node.expr.has_index {
				g.write('(${s})')
			}
		}
		g.expr(ast.Expr(node.expr))
	} else {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		if expr_type.has_flag(.option_mut_param_t) {
			g.write('*')
		}
		for {
			if node.expr is ast.Ident {
				if node.expr.obj is ast.Var {
					if node.expr.obj.ct_type_var == .field_var && g.comptime.inside_comptime_for
						&& (node.expr.obj.ct_type_unwrapped || node.expr.obj.is_unwrapped) {
						field_type := g.comptime.comptime_for_field_type
						if field_type.has_flag(.option) {
							styp := g.base_type(field_type.clear_flag(.option))
							is_auto_heap := node.expr.is_auto_heap()
							ptr := if is_auto_heap { '->' } else { '.' }
							g.write('(*(${styp}*)${c_name(node.expr.name)}${ptr}data)')
							break
						}
					}
				}
			}
			g.expr(ast.Expr(node.expr))
			break
		}
		g.inside_opt_or_res = old_inside_opt_or_res
	}
	g.write(')')
	if (g.inside_assign || g.expected_fixed_arr) && !expr_type.has_option_or_result()
		&& g.table.final_sym(expr_type).kind == .array_fixed {
		g.write('.ret_arr')
	}
}

fn (mut g Gen) dump_expr_definitions() {
	mut dump_already_generated_fns := map[string]bool{}
	mut dump_typedefs := map[string]bool{}
	mut dump_fns := strings.new_builder(100)
	mut dump_fn_defs := strings.new_builder(100)
	for dump_type, cname in g.table.dumps {
		dump_sym := g.table.sym(ast.idx_to_type(dump_type))
		// eprintln('>>> dump_type: ${dump_type} | cname: ${cname} | dump_sym: ${dump_sym.name}')
		mut name := cname
		if dump_sym.language == .c {
			name = name[3..]
		}
		_, str_method_expects_ptr, _ := dump_sym.str_method_info()
		typ := ast.idx_to_type(dump_type)
		if g.pref.skip_unused
			&& (!g.table.used_features.dump || typ.idx() !in g.table.used_features.used_syms) {
			continue
		}
		is_ptr := typ.is_ptr()
		deref, _ := deref_kind(str_method_expects_ptr, is_ptr, typ)
		to_string_fn_name := g.get_str_fn(typ.clear_flags(.shared_f, .result))
		is_option := typ.has_option_or_result()
		mut ptr_asterisk := if is_ptr { '*'.repeat(typ.nr_muls()) } else { '' }
		mut str_dumparg_type := ''
		mut str_dumparg_ret_type := ''
		if dump_sym.kind == .none {
			str_dumparg_type = 'IError' + ptr_asterisk
		} else if dump_sym.kind == .function {
			if is_option {
				ptr_asterisk = ptr_asterisk.replace('*', '_ptr')
			}
			str_dumparg_type += g.styp(typ).replace('*', '') + ptr_asterisk
		} else {
			if is_option {
				str_dumparg_type += '_option_'
				ptr_asterisk = ptr_asterisk.replace('*', '_ptr')
			}
			str_dumparg_type += g.cc_type(typ, true) + ptr_asterisk
		}
		mut is_fixed_arr_ret := false
		if dump_sym.info is ast.FnType && !is_option {
			str_dumparg_type = 'DumpFNType_${name}'
			tdef_pos := g.out.len
			g.write_fn_ptr_decl(&dump_sym.info, str_dumparg_type)
			str_tdef := g.out.after(tdef_pos)
			g.go_back(str_tdef.len)
			dump_typedefs['typedef ${str_tdef};'] = true
			str_dumparg_ret_type = str_dumparg_type
		} else if !is_option && dump_sym.is_array_fixed() {
			match dump_sym.kind {
				.array_fixed {
					if (dump_sym.info as ast.ArrayFixed).is_fn_ret {
						str_dumparg_ret_type = str_dumparg_type
						str_dumparg_type = str_dumparg_type.trim_string_left('_v_')
					} else {
						// fixed array returned from function
						str_dumparg_ret_type = '_v_' + str_dumparg_type
					}
				}
				.alias {
					parent_sym := g.table.sym((dump_sym.info as ast.Alias).parent_type)
					if parent_sym.kind == .array_fixed {
						str_dumparg_ret_type =
							if (parent_sym.info as ast.ArrayFixed).is_fn_ret { '' } else { '_v_' } +
							g.cc_type((dump_sym.info as ast.Alias).parent_type, true)
						str_dumparg_type = str_dumparg_ret_type.trim_string_left('_v_')
						is_fixed_arr_ret = true
					}
				}
				else {}
			}
			is_fixed_arr_ret = true
		} else {
			str_dumparg_ret_type = str_dumparg_type
		}
		dump_fn_name := '_v_dump_expr_${name}' +
			(if is_ptr { '__ptr'.repeat(typ.nr_muls()) } else { '' })

		// protect against duplicate declarations:
		if dump_already_generated_fns[dump_fn_name] {
			continue
		}
		dump_already_generated_fns[dump_fn_name] = true

		dump_fn_defs.writeln('${str_dumparg_ret_type} ${dump_fn_name}(string fpath, ${ast.int_type_name} line, string sexpr, ${str_dumparg_type} dump_arg);')
		if g.writeln_fn_header('${str_dumparg_ret_type} ${dump_fn_name}(string fpath, ${ast.int_type_name} line, string sexpr, ${str_dumparg_type} dump_arg)', mut
			dump_fns)
		{
			continue
		}
		mut surrounder := util.new_surrounder(3)
		int_str := g.get_str_fn(ast.int_type)
		surrounder.add('\tstring sline = ${int_str}(line);', '\tbuiltin__string_free(&sline);')
		if dump_sym.kind == .function && !is_option {
			surrounder.add('\tstring value = ${to_string_fn_name}();', '\tbuiltin__string_free(&value);')
		} else if dump_sym.kind == .none {
			surrounder.add('\tstring value = _S("none");', '\tbuiltin__string_free(&value);')
		} else if is_ptr {
			if typ.has_flag(.option) {
				surrounder.add('\tstring value = builtin__isnil(&dump_arg.data) ? _S("nil") : ${to_string_fn_name}(${deref}dump_arg);',
					'\tbuiltin__string_free(&value);')
			} else {
				prefix := if dump_sym.is_c_struct() {
					c_struct_ptr(dump_sym, typ, str_method_expects_ptr)
				} else {
					deref
				}
				surrounder.add('\tstring value = (dump_arg == NULL) ? _S("nil") : ${to_string_fn_name}(${prefix}dump_arg);',
					'\tbuiltin__string_free(&value);')
			}
		} else {
			prefix := if dump_sym.is_c_struct() {
				c_struct_ptr(dump_sym, typ, str_method_expects_ptr)
			} else {
				deref
			}
			surrounder.add('\tstring value = ${to_string_fn_name}(${prefix}dump_arg);',
				'\tbuiltin__string_free(&value);')
		}
		surrounder.add('
	strings__Builder sb = strings__new_builder(64);
', '
	string res;
	res = strings__Builder_str(&sb);
	builtin__eprint(res);
	builtin__string_free(&res);
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
		if is_fixed_arr_ret && !is_ptr {
			tmp_var := g.new_tmp_var()
			init_str := if dump_sym.is_empty_struct_array() {
				'{E_STRUCT}'
			} else {
				'{0}'
			}
			dump_fns.writeln('\t${str_dumparg_ret_type} ${tmp_var} = ${init_str};')
			dump_fns.writeln('\tmemcpy(${tmp_var}.ret_arr, dump_arg, sizeof(${str_dumparg_type}));')
			dump_fns.writeln('\treturn ${tmp_var};')
		} else {
			dump_fns.writeln('\treturn dump_arg; /* ${str_dumparg_type} */')
		}
		dump_fns.writeln('}')
	}
	for tdef, _ in dump_typedefs {
		g.definitions.writeln(tdef)
	}
	if dump_fn_defs.len > 0 {
		g.definitions.writeln(dump_fn_defs.str())
		g.dump_funcs.writeln(dump_fns.str())
	}
}

fn (mut g Gen) writeln_fn_header(s string, mut sb strings.Builder) bool {
	if g.pref.build_mode == .build_module {
		sb.writeln('${s};')
		return true
	}
	sb.writeln('${s} {')
	return false
}
