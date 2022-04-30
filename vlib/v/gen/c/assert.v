// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) gen_assert_stmt(original_assert_statement ast.AssertStmt) {
	if !original_assert_statement.is_used {
		return
	}
	mut node := original_assert_statement
	g.writeln('// assert')
	if mut node.expr is ast.InfixExpr {
		if subst_expr := g.assert_subexpression_to_ctemp(node.expr.left, node.expr.left_type) {
			node.expr.left = subst_expr
		}
		if subst_expr := g.assert_subexpression_to_ctemp(node.expr.right, node.expr.right_type) {
			node.expr.right = subst_expr
		}
	}
	g.inside_ternary++
	if g.pref.is_test {
		g.write('if (')
		g.expr(node.expr)
		g.write(')')
		g.decrement_inside_ternary()
		g.writeln(' {')
		metaname_ok := g.gen_assert_metainfo(node)
		g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_assert_pass(test_runner._object, &$metaname_ok);')
		g.writeln('} else {')
		metaname_fail := g.gen_assert_metainfo(node)
		g.writeln('\tmain__TestRunner_name_table[test_runner._typ]._method_assert_fail(test_runner._object, &$metaname_fail);')
		g.gen_assert_postfailure_mode(node)
		g.writeln('\tlongjmp(g_jump_buffer, 1);')
		g.writeln('\t// TODO')
		g.writeln('\t// Maybe print all vars in a test function if it fails?')
		g.writeln('}')
	} else {
		g.write('if (!(')
		g.expr(node.expr)
		g.write('))')
		g.decrement_inside_ternary()
		g.writeln(' {')
		metaname_panic := g.gen_assert_metainfo(node)
		g.writeln('\t__print_assert_failure(&$metaname_panic);')
		g.gen_assert_postfailure_mode(node)
		g.writeln('\t_v_panic(_SLIT("Assertion failed..."));')
		g.writeln('}')
	}
}

struct UnsupportedAssertCtempTransform {
	Error
}

const unsupported_ctemp_assert_transform = IError(UnsupportedAssertCtempTransform{})

fn (mut g Gen) assert_subexpression_to_ctemp(expr ast.Expr, expr_type ast.Type) ?ast.Expr {
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
				if sym.kind == .struct_ {
					if (sym.info as ast.Struct).is_union {
						return c.unsupported_ctemp_assert_transform
					}
				}
				return g.new_ctemp_var_then_gen(expr, expr_type)
			}
		}
		else {}
	}
	return c.unsupported_ctemp_assert_transform
}

fn (mut g Gen) gen_assert_postfailure_mode(node ast.AssertStmt) {
	g.write_v_source_line_info(node.pos)
	match g.pref.assert_failure_mode {
		.default {}
		.aborts {
			g.writeln('\tabort();')
		}
		.backtraces {
			g.writeln('\tprint_backtrace();')
		}
	}
}

fn (mut g Gen) gen_assert_metainfo(node ast.AssertStmt) string {
	mod_path := cestring(g.file.path)
	fn_name := g.fn_decl.name
	line_nr := node.pos.line_nr
	src := cestring(node.expr.str())
	metaname := 'v_assert_meta_info_$g.new_tmp_var()'
	g.writeln('\tVAssertMetaInfo $metaname = {0};')
	g.writeln('\t${metaname}.fpath = ${ctoslit(mod_path)};')
	g.writeln('\t${metaname}.line_nr = $line_nr;')
	g.writeln('\t${metaname}.fn_name = ${ctoslit(fn_name)};')
	metasrc := cnewlines(ctoslit(src))
	g.writeln('\t${metaname}.src = $metasrc;')
	match node.expr {
		ast.InfixExpr {
			expr_op_str := ctoslit(node.expr.op.str())
			expr_left_str := cnewlines(ctoslit(node.expr.left.str()))
			expr_right_str := cnewlines(ctoslit(node.expr.right.str()))
			g.writeln('\t${metaname}.op = $expr_op_str;')
			g.writeln('\t${metaname}.llabel = $expr_left_str;')
			g.writeln('\t${metaname}.rlabel = $expr_right_str;')
			g.write('\t${metaname}.lvalue = ')
			g.gen_assert_single_expr(node.expr.left, node.expr.left_type)
			g.writeln(';')
			g.write('\t${metaname}.rvalue = ')
			g.gen_assert_single_expr(node.expr.right, node.expr.right_type)
			g.writeln(';')
		}
		ast.CallExpr {
			g.writeln('\t${metaname}.op = _SLIT("call");')
		}
		else {}
	}
	return metaname
}

fn (mut g Gen) gen_assert_single_expr(expr ast.Expr, typ ast.Type) {
	// eprintln('> gen_assert_single_expr typ: $typ | expr: $expr | typeof(expr): ${typeof(expr)}')
	unknown_value := '*unknown value*'
	match expr {
		ast.CastExpr, ast.IfExpr, ast.IndexExpr, ast.MatchExpr {
			g.write(ctoslit(unknown_value))
		}
		ast.PrefixExpr {
			if expr.right is ast.CastExpr {
				// TODO: remove this check;
				// vlib/builtin/map_test.v (a map of &int, set to &int(0)) fails
				// without special casing ast.CastExpr here
				g.write(ctoslit(unknown_value))
			} else {
				g.gen_expr_to_string(expr, typ)
			}
		}
		ast.TypeNode {
			sym := g.table.sym(g.unwrap_generic(typ))
			g.write(ctoslit('$sym.name'))
		}
		else {
			mut should_clone := true
			if typ == ast.string_type && expr is ast.StringLiteral {
				should_clone = false
			}
			if expr is ast.CTempVar {
				if expr.orig is ast.CallExpr {
					should_clone = false
					if expr.orig.or_block.kind == .propagate_option {
						should_clone = true
					}
					if expr.orig.is_method && expr.orig.args.len == 0
						&& expr.orig.name == 'type_name' {
						should_clone = true
					}
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
	g.write(' /* typeof: ' + expr.type_name() + ' type: ' + typ.str() + ' */ ')
}
