// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

enum AssertMetainfoKind {
	pass
	fail
	panic
}

fn (mut g Gen) assert_stmt(original_assert_statement ast.AssertStmt) {
	if !original_assert_statement.is_used {
		return
	}
	mut node := original_assert_statement
	g.writeln('// assert')

	mut save_left := ast.empty_expr
	mut save_right := ast.empty_expr

	if mut node.expr is ast.InfixExpr {
		if subst_expr := g.assert_subexpression_to_ctemp(node.expr.left, node.expr.left_type) {
			save_left = node.expr.left
			node.expr.left = subst_expr
		}
		if subst_expr := g.assert_subexpression_to_ctemp(node.expr.right, node.expr.right_type) {
			save_right = node.expr.right
			node.expr.right = subst_expr
		}
	}
	g.inside_ternary++
	if g.pref.is_test {
		g.write('if (')
		prev_inside_ternary := g.inside_ternary
		g.inside_ternary = 0
		g.expr(node.expr)
		g.inside_ternary = prev_inside_ternary
		g.write(')')
		g.decrement_inside_ternary()
		g.writeln(' {')
		metaname_ok := g.gen_assert_metainfo(node, .pass)
		g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_assert_pass(test_runner._object, &${metaname_ok});')
		g.writeln('} else {')
		metaname_fail := g.gen_assert_metainfo(node, .fail)
		g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_assert_fail(test_runner._object, &${metaname_fail});')
		g.gen_assert_postfailure_mode(node)
		g.writeln('}')
	} else {
		g.write('if (!(')
		prev_inside_ternary := g.inside_ternary
		g.inside_ternary = 0
		g.expr(node.expr)
		g.inside_ternary = prev_inside_ternary
		g.write('))')
		g.decrement_inside_ternary()
		g.writeln(' {')
		metaname_panic := g.gen_assert_metainfo(node, .panic)
		g.writeln('\t__print_assert_failure(&${metaname_panic});')
		g.gen_assert_postfailure_mode(node)
		g.writeln('}')
	}

	if mut node.expr is ast.InfixExpr {
		if node.expr.left is ast.CTempVar {
			node.expr.left = save_left
		}
		if node.expr.right is ast.CTempVar {
			node.expr.right = save_right
		}
	}
}

struct UnsupportedAssertCtempTransform {
	Error
}

const unsupported_ctemp_assert_transform = IError(UnsupportedAssertCtempTransform{})

fn (mut g Gen) assert_subexpression_to_ctemp(expr ast.Expr, expr_type ast.Type) !ast.Expr {
	match expr {
		ast.CallExpr {
			return g.new_ctemp_var_then_gen(expr, expr_type)
		}
		ast.ParExpr {
			if expr.expr is ast.CallExpr {
				return g.new_ctemp_var_then_gen(expr.expr, expr_type)
			}
		}
		ast.SelectorExpr {
			if expr.expr is ast.CallExpr {
				sym := g.table.final_sym(g.unwrap_generic(expr.expr.return_type))
				if sym.kind == .struct {
					if (sym.info as ast.Struct).is_union {
						return unsupported_ctemp_assert_transform
					}
				}
				return g.new_ctemp_var_then_gen(expr, expr_type)
			}
		}
		else {}
	}
	return unsupported_ctemp_assert_transform
}

fn (mut g Gen) gen_assert_postfailure_mode(node ast.AssertStmt) {
	g.write_v_source_line_info_stmt(node)
	if g.pref.assert_failure_mode == .continues
		|| g.fn_decl.attrs.any(it.name == 'assert_continues') {
		return
	}
	if g.pref.assert_failure_mode == .aborts || g.fn_decl.attrs.any(it.name == 'assert_aborts') {
		g.writeln('\tabort();')
	}
	if g.pref.assert_failure_mode == .backtraces
		|| g.fn_decl.attrs.any(it.name == 'assert_backtraces') {
		if _ := g.table.fns['print_backtrace'] {
			g.writeln('\tprint_backtrace();')
		}
	}
	if g.pref.is_test {
		g.writeln('\tlongjmp(g_jump_buffer, 1);')
	}
	g.writeln2('\t// TODO', '\t// Maybe print all vars in a test function if it fails?')
	if g.pref.assert_failure_mode != .continues {
		g.writeln('\t_v_panic(_S("Assertion failed..."));')
	}
}

fn (mut g Gen) gen_assert_metainfo(node ast.AssertStmt, kind AssertMetainfoKind) string {
	mod_path := cestring(g.file.path)
	fn_name := g.fn_decl.name
	line_nr := node.pos.line_nr
	mut src := node.expr.str()
	if node.extra !is ast.EmptyExpr {
		src += ', ' + node.extra.str()
	}
	src = cestring(src)
	metaname := 'v_assert_meta_info_${g.new_tmp_var()}'
	g.writeln('\tVAssertMetaInfo ${metaname} = {0};')
	g.writeln('\t${metaname}.fpath = ${ctoslit(mod_path)};')
	g.writeln('\t${metaname}.line_nr = ${line_nr};')
	g.writeln('\t${metaname}.fn_name = ${ctoslit(fn_name)};')
	metasrc := cnewlines(ctoslit(src))
	g.writeln('\t${metaname}.src = ${metasrc};')
	match node.expr {
		ast.InfixExpr {
			expr_op_str := ctoslit(node.expr.op.str())
			expr_left_str := cnewlines(ctoslit(node.expr.left.str()))
			expr_right_str := cnewlines(ctoslit(node.expr.right.str()))
			g.writeln('\t${metaname}.op = ${expr_op_str};')
			g.writeln('\t${metaname}.llabel = ${expr_left_str};')
			g.writeln('\t${metaname}.rlabel = ${expr_right_str};')
			left_type := if node.expr.left_ct_expr {
				g.type_resolver.get_type_or_default(node.expr.left, node.expr.left_type)
			} else {
				node.expr.left_type
			}
			right_type := if node.expr.right_ct_expr {
				g.type_resolver.get_type(node.expr.right)
			} else {
				node.expr.right_type
			}
			if kind != .pass {
				g.write('\t${metaname}.lvalue = ')
				g.gen_assert_single_expr(node.expr.left, left_type)
				g.writeln(';')
				g.write('\t${metaname}.rvalue = ')
				g.gen_assert_single_expr(node.expr.right, right_type)
				g.writeln(';')
			}
		}
		ast.CallExpr {
			g.writeln('\t${metaname}.op = _S("call");')
		}
		else {}
	}
	if node.extra is ast.EmptyExpr {
		g.writeln('\t${metaname}.has_msg = false;')
		g.writeln('\t${metaname}.message = _SLIT0;')
	} else {
		g.writeln('\t${metaname}.has_msg = true;')
		g.write('\t${metaname}.message = ')
		g.gen_assert_single_expr(node.extra, ast.string_type)
		g.writeln(';')
	}
	return metaname
}

fn (mut g Gen) gen_assert_single_expr(expr ast.Expr, typ ast.Type) {
	// eprintln('> gen_assert_single_expr typ: $typ | expr: $expr | typeof(expr): ${typeof(expr)}')
	expr_str := '${expr}'
	match expr {
		ast.CastExpr {
			if typ.is_float() || g.table.final_sym(typ).is_float() {
				g.gen_expr_to_string(expr.expr, typ)
			} else {
				g.write(ctoslit(expr_str))
			}
		}
		ast.ParExpr {
			g.gen_assert_single_expr(expr.expr, typ)
		}
		ast.IfExpr, ast.MatchExpr, ast.RangeExpr {
			g.write(ctoslit(expr_str))
		}
		ast.IndexExpr {
			g.gen_expr_to_string(expr, typ)
		}
		ast.PrefixExpr {
			if expr.right is ast.CastExpr {
				// TODO: remove this check;
				// vlib/builtin/map_test.v (a map of &int, set to &int(0)) fails
				// without special casing ast.CastExpr here
				g.write(ctoslit(expr_str))
			} else if expr.right is ast.Ident {
				g.write(ctoslit(expr_str))
			} else {
				g.gen_expr_to_string(expr, typ)
			}
		}
		ast.TypeNode {
			sym := g.table.sym(g.unwrap_generic(typ))
			g.write(ctoslit('${sym.name}'))
		}
		else {
			mut should_clone := true
			if typ == ast.string_type
				&& expr in [ast.IndexExpr, ast.CallExpr, ast.StringLiteral, ast.StringInterLiteral] {
				should_clone = false
			}
			if expr is ast.CTempVar && expr.orig is ast.CallExpr {
				should_clone = false
				if expr.orig.or_block.kind == .propagate_option {
					should_clone = true
				}
				if expr.orig.is_method && expr.orig.args.len == 0 && expr.orig.name == 'type_name' {
					should_clone = true
				}
			}
			if should_clone {
				g.write('string_clone(')
			}
			g.gen_expr_to_string(expr, typ)
			if should_clone {
				g.write(')')
			}
		}
	}
}
