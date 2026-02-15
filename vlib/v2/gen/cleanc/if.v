// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

fn (mut g Gen) infer_tuple_field_types_from_if_expr(expr ast.IfExpr) ?[]string {
	if fields := g.infer_tuple_field_types_from_stmts(expr.stmts) {
		return fields
	}
	if expr.else_expr is ast.IfExpr {
		if fields := g.infer_tuple_field_types_from_if_expr(expr.else_expr) {
			return fields
		}
	} else if expr.else_expr is ast.UnsafeExpr {
		unsafe_expr := expr.else_expr as ast.UnsafeExpr
		if fields := g.infer_tuple_field_types_from_stmts(unsafe_expr.stmts) {
			return fields
		}
	}
	return none
}

fn (mut g Gen) gen_decl_if_expr_branch(name string, stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				nested_if := stmt.expr as ast.IfExpr
				if !g.if_expr_can_be_ternary(nested_if) && nested_if.else_expr !is ast.EmptyExpr {
					g.gen_decl_if_expr(name, nested_if)
					continue
				}
			}
			g.write_indent()
			g.sb.write_string('${name} = ')
			g.expr(stmt.expr)
			g.sb.writeln(';')
		} else {
			g.gen_stmt(stmt)
		}
	}
}

fn (mut g Gen) gen_decl_if_expr(name string, if_expr ast.IfExpr) {
	if if_expr.cond is ast.EmptyExpr {
		g.gen_decl_if_expr_branch(name, if_expr.stmts)
		return
	}
	g.write_indent()
	g.sb.write_string('if (')
	g.expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_decl_if_expr_branch(name, if_expr.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_decl_if_expr_branch(name, else_if.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_decl_if_expr(name, else_if)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
	} else if if_expr.else_expr !is ast.EmptyExpr {
		g.sb.writeln(' else {')
		g.indent++
		g.write_indent()
		g.sb.write_string('${name} = ')
		g.expr(if_expr.else_expr)
		g.sb.writeln(';')
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	} else {
		g.sb.writeln('')
	}
}

fn (mut g Gen) gen_return_if_branch(stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	for i, stmt in stmts {
		if i == stmts.len - 1 && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				nested_if := stmt.expr as ast.IfExpr
				if !g.if_expr_can_be_ternary(nested_if) && nested_if.else_expr !is ast.EmptyExpr {
					g.gen_return_if_expr(nested_if, true)
					continue
				}
			}
			mut ret_exprs := []ast.Expr{cap: 1}
			ret_exprs << stmt.expr
			ret_stmt := ast.ReturnStmt{
				exprs: ret_exprs
			}
			g.gen_stmt(ret_stmt)
		} else {
			g.gen_stmt(stmt)
		}
	}
}

fn (mut g Gen) gen_return_if_expr(if_expr ast.IfExpr, emit_indent bool) {
	if if_expr.cond is ast.EmptyExpr {
		g.gen_return_if_branch(if_expr.stmts)
		return
	}
	if emit_indent {
		g.write_indent()
	}
	g.sb.write_string('if (')
	g.expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_return_if_branch(if_expr.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_return_if_branch(else_if.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_return_if_expr(else_if, true)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
	} else if if_expr.else_expr !is ast.EmptyExpr {
		g.sb.writeln(' else {')
		g.indent++
		mut else_exprs := []ast.Expr{cap: 1}
		else_exprs << if_expr.else_expr
		else_stmt := ast.ReturnStmt{
			exprs: else_exprs
		}
		g.gen_stmt(else_stmt)
		g.indent--
		g.write_indent()
		g.sb.writeln('}')
	} else {
		g.sb.writeln('')
	}
}

fn (g &Gen) if_expr_can_be_ternary(node ast.IfExpr) bool {
	if node.cond !is ast.EmptyExpr {
		if node.stmts.len != 1 || node.stmts[0] !is ast.ExprStmt {
			return false
		}
		if node.else_expr is ast.EmptyExpr {
			return false
		}
	}
	if node.cond is ast.EmptyExpr {
		return node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt
	}
	if node.else_expr is ast.IfExpr {
		return g.if_expr_can_be_ternary(node.else_expr)
	}
	return true
}

fn (g &Gen) extract_if_expr(expr ast.Expr) ?ast.IfExpr {
	match expr {
		ast.IfExpr {
			return expr
		}
		ast.ParenExpr {
			return g.extract_if_expr(expr.expr)
		}
		ast.ModifierExpr {
			return g.extract_if_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 1 {
				stmt0 := expr.stmts[0]
				if stmt0 is ast.ExprStmt {
					expr_stmt := stmt0 as ast.ExprStmt
					return g.extract_if_expr(expr_stmt.expr)
				}
			}
			return none
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) gen_if_expr_stmt(node ast.IfExpr) {
	// Skip empty conditions (pure else blocks shouldn't appear at top level)
	if node.cond is ast.EmptyExpr {
		return
	}
	g.sb.write_string('if (')
	g.expr(node.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_stmts(node.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	// Handle else / else-if
	if node.else_expr !is ast.EmptyExpr {
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				g.sb.writeln(' else {')
				g.indent++
				g.gen_stmts(else_if.stmts)
				g.indent--
				g.write_indent()
				g.sb.write_string('}')
			} else {
				g.sb.write_string(' else ')
				g.gen_if_expr_stmt(else_if)
			}
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_stmts_from_expr(node.else_expr)
			g.indent--
			g.write_indent()
			g.sb.write_string('}')
		}
	}
	g.sb.writeln('')
}

fn (mut g Gen) gen_if_expr_ternary(node ast.IfExpr) {
	if node.cond is ast.EmptyExpr {
		if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
			stmt := node.stmts[0] as ast.ExprStmt
			g.expr(stmt.expr)
		} else {
			g.sb.write_string('0')
		}
		return
	}
	g.sb.write_string('(')
	g.expr(node.cond)
	g.sb.write_string(' ? ')
	if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
		stmt := node.stmts[0] as ast.ExprStmt
		if nested := g.extract_if_expr(stmt.expr) {
			g.gen_if_expr_value(nested)
		} else {
			g.expr(stmt.expr)
		}
	} else {
		g.sb.write_string('0')
	}
	g.sb.write_string(' : ')
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		g.gen_if_expr_value(else_if)
	} else if node.else_expr is ast.EmptyExpr {
		g.sb.write_string('0')
	} else {
		if nested := g.extract_if_expr(node.else_expr) {
			g.gen_if_expr_value(nested)
		} else {
			g.expr(node.else_expr)
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_if_expr_value(node ast.IfExpr) {
	mut value_type := g.infer_if_expr_type(node)
	if g.if_expr_can_be_ternary(node) && value_type != '' && value_type != 'void' {
		g.gen_if_expr_ternary(node)
		return
	}
	if value_type == '' || value_type == 'void' {
		g.sb.write_string('({ ')
		g.gen_if_expr_stmt(node)
		g.sb.write_string('; 0; })')
		return
	}
	if value_type == 'int_literal' {
		value_type = 'int'
	}
	tmp_name := '_if_expr_t${g.tmp_counter}'
	g.tmp_counter++
	g.sb.write_string('({ ${value_type} ${tmp_name} = ${zero_value_for_type(value_type)}; ')
	g.gen_decl_if_expr(tmp_name, node)
	g.sb.write_string(' ${tmp_name}; })')
}

fn (mut g Gen) infer_if_expr_type(node ast.IfExpr) string {
	if t := g.get_expr_type_from_env(node) {
		if t != '' {
			return t
		}
	}
	if node.stmts.len == 1 && node.stmts[0] is ast.ExprStmt {
		stmt := node.stmts[0] as ast.ExprStmt
		t := g.get_expr_type(stmt.expr)
		if t != '' && t != 'int' {
			return t
		}
	}
	if node.else_expr is ast.IfExpr {
		t := g.infer_if_expr_type(node.else_expr)
		if t != '' {
			return t
		}
	} else if node.else_expr !is ast.EmptyExpr {
		t := g.get_expr_type(node.else_expr)
		if t != '' {
			return t
		}
	}
	return 'int'
}

// expr_produces_pointer checks if an expression produces a pointer value in C.
// More comprehensive than expr_is_pointer - handles casts, pointer arithmetic, etc.
// Used in gen_call_arg to avoid wrapping pointer expressions in compound literals.
