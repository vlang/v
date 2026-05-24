// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

fn const_bool_value(expr ast.Expr) ?bool {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .key_true {
				return true
			}
			if expr.kind == .key_false {
				return false
			}
			if expr.value == 'true' {
				return true
			}
			if expr.value == 'false' {
				return false
			}
		}
		ast.Ident {
			if expr.name == 'true' {
				return true
			}
			if expr.name == 'false' {
				return false
			}
		}
		ast.Keyword {
			if expr.tok == .key_true {
				return true
			}
			if expr.tok == .key_false {
				return false
			}
		}
		ast.ParenExpr {
			return const_bool_value(expr.expr)
		}
		ast.ModifierExpr {
			return const_bool_value(expr.expr)
		}
		ast.CastExpr {
			return const_bool_value(expr.expr)
		}
		ast.ComptimeExpr {
			return const_bool_value(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 1 && expr.stmts[0] is ast.ExprStmt {
				stmt := expr.stmts[0] as ast.ExprStmt
				return const_bool_value(stmt.expr)
			}
		}
		ast.PrefixExpr {
			if expr.op == .not {
				if v := const_bool_value(expr.expr) {
					return !v
				}
			}
		}
		else {}
	}

	return none
}

fn (mut g Gen) gen_if_expr_assign_value(name string, value_type string, expr ast.Expr) {
	g.write_indent()
	g.sb.write_string('${name} = ')
	if g.get_sum_type_variants_for(value_type).len > 0 {
		g.gen_type_cast_expr(value_type, expr)
	} else {
		g.expr(expr)
	}
	g.sb.writeln(';')
}

fn (mut g Gen) gen_decl_if_expr_branch(name string, value_type string, stmts []ast.Stmt) {
	last_idx := last_non_empty_stmt_index(stmts)
	if last_idx < 0 {
		return
	}
	for i, stmt in stmts {
		if stmt is ast.EmptyStmt {
			continue
		}
		if i == last_idx && stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				nested_if := stmt.expr as ast.IfExpr
				if !g.if_expr_can_be_ternary(&nested_if) && nested_if.else_expr !is ast.EmptyExpr {
					g.gen_decl_if_expr(name, value_type, &nested_if)
					continue
				}
				if nested_if.else_expr is ast.EmptyExpr {
					if payload_expr := g.unsafe_expr_or_payload_value(stmts) {
						g.gen_stmt(stmt)
						g.gen_if_expr_assign_value(name, value_type, payload_expr)
						continue
					}
				}
			}
			// When the last expression is a void call (e.g. array__push from <<),
			// emit it as a standalone statement without assigning to the temp var.
			if g.expr_is_void_call(stmt.expr) {
				g.write_indent()
				g.expr(stmt.expr)
				g.sb.writeln(';')
			} else {
				g.gen_if_expr_assign_value(name, value_type, stmt.expr)
			}
		} else {
			g.gen_stmt(stmt)
		}
	}
}

fn (mut g Gen) gen_scoped_decl_if_expr_branch(name string, value_type string, stmts []ast.Stmt) {
	saved_runtime_local_types := g.runtime_local_types.clone()
	saved_runtime_decl_types := g.runtime_decl_types.clone()
	saved_not_local_var_cache := g.not_local_var_cache.clone()
	g.gen_decl_if_expr_branch(name, value_type, stmts)
	g.runtime_local_types = saved_runtime_local_types.clone()
	g.runtime_decl_types = saved_runtime_decl_types.clone()
	g.not_local_var_cache = saved_not_local_var_cache.clone()
}

// expr_is_void_call checks if the expression is a function call that returns void.
// Used to prevent assigning void results to temp variables in if-expression branches.
fn (mut g Gen) expr_is_void_call(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		if ret := g.get_call_return_type(expr.lhs, expr.args) {
			return ret == 'void'
		}
		// Check resolved name
		c_name := g.resolve_call_name(expr.lhs, expr.args.len)
		if c_name != '' {
			if ret := g.fn_return_types[c_name] {
				return ret == 'void'
			}
		}
		// panic/v_panic always returns void (noreturn)
		if expr.lhs is ast.Ident {
			if expr.lhs.name in ['panic', 'v_panic'] {
				return true
			}
		}
	}
	return false
}

fn (mut g Gen) gen_decl_if_expr(name string, value_type string, if_expr &ast.IfExpr) {
	if if_expr.cond is ast.EmptyExpr {
		g.gen_scoped_decl_if_expr_branch(name, value_type, if_expr.stmts)
		return
	}
	g.write_indent()
	g.sb.write_string('if (')
	g.expr(if_expr.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_scoped_decl_if_expr_branch(name, value_type, if_expr.stmts)
	g.indent--
	g.write_indent()
	g.sb.write_string('}')
	if if_expr.else_expr is ast.IfExpr {
		else_if := if_expr.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_scoped_decl_if_expr_branch(name, value_type, else_if.stmts)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_decl_if_expr(name, value_type, &else_if)
			g.indent--
			g.write_indent()
			g.sb.writeln('}')
		}
	} else if if_expr.else_expr !is ast.EmptyExpr {
		g.sb.writeln(' else {')
		g.indent++
		g.gen_if_expr_assign_value(name, value_type, if_expr.else_expr)
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
				if !g.if_expr_can_be_ternary(&nested_if) && nested_if.else_expr !is ast.EmptyExpr {
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

fn (g &Gen) if_expr_can_be_ternary(node &ast.IfExpr) bool {
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
		else_if := node.else_expr as ast.IfExpr
		return g.if_expr_can_be_ternary(&else_if)
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

fn last_non_empty_stmt_index(stmts []ast.Stmt) int {
	mut idx := stmts.len - 1
	for idx >= 0 && stmts[idx] is ast.EmptyStmt {
		idx--
	}
	return idx
}

fn (mut g Gen) branch_result_type(stmts []ast.Stmt) string {
	last_idx := last_non_empty_stmt_index(stmts)
	if last_idx < 0 {
		return ''
	}
	last := stmts[last_idx]
	match last {
		ast.BlockStmt {
			return g.branch_result_type(last.stmts)
		}
		ast.ExprStmt {
			if nested := g.extract_if_expr(last.expr) {
				return g.get_if_expr_type(&nested)
			}
			mut typ := g.get_expr_type(last.expr)
			if typ == '' || typ == 'int' || typ == 'void*' || typ == 'voidptr' {
				if raw := g.get_raw_type(last.expr) {
					raw_type := g.types_type_to_c(raw)
					if raw_type != '' && raw_type != 'void*' && raw_type != 'voidptr' {
						typ = raw_type
					}
				}
			}
			return typ
		}
		else {
			return ''
		}
	}
}

fn (mut g Gen) gen_if_expr_stmt(node &ast.IfExpr) {
	// Skip empty conditions (pure else blocks shouldn't appear at top level)
	if node.cond is ast.EmptyExpr {
		return
	}
	if cond_value := const_bool_value(node.cond) {
		if cond_value {
			g.gen_scoped_stmts(node.stmts)
			return
		}
		if node.else_expr is ast.IfExpr {
			else_if := node.else_expr as ast.IfExpr
			if else_if.cond is ast.EmptyExpr {
				g.gen_scoped_stmts(else_if.stmts)
			} else {
				g.gen_if_expr_stmt(&else_if)
			}
		} else if node.else_expr !is ast.EmptyExpr {
			g.gen_scoped_expr_stmts(node.else_expr)
		}
		return
	}
	g.sb.write_string('if (')
	g.expr(node.cond)
	g.sb.writeln(') {')
	g.indent++
	g.gen_scoped_stmts(node.stmts)
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
				g.gen_scoped_stmts(else_if.stmts)
				g.indent--
				g.write_indent()
				g.sb.write_string('}')
			} else {
				g.sb.write_string(' else ')
				g.gen_if_expr_stmt(&else_if)
			}
		} else {
			g.sb.writeln(' else {')
			g.indent++
			g.gen_scoped_expr_stmts(node.else_expr)
			g.indent--
			g.write_indent()
			g.sb.write_string('}')
		}
	}
	g.sb.writeln('')
}

fn (mut g Gen) gen_if_expr_ternary(node &ast.IfExpr) {
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
			g.gen_if_expr_value(&nested)
		} else {
			g.expr(stmt.expr)
		}
	} else {
		g.sb.write_string('0')
	}
	g.sb.write_string(' : ')
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		g.gen_if_expr_value(&else_if)
	} else if node.else_expr is ast.EmptyExpr {
		g.sb.write_string('0')
	} else {
		if nested := g.extract_if_expr(node.else_expr) {
			g.gen_if_expr_value(&nested)
		} else {
			g.expr(node.else_expr)
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_if_expr_value(node &ast.IfExpr) {
	mut value_type := g.get_if_expr_type(node)
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
	g.gen_decl_if_expr(tmp_name, value_type, node)
	g.sb.write_string(' ${tmp_name}; })')
}

fn (mut g Gen) get_if_expr_type(node &ast.IfExpr) string {
	mut env_type := ''
	if t := g.get_expr_type_from_env(ast.Expr(*node)) {
		if t != '' {
			env_type = t
		}
	}
	mut branch_type := g.branch_result_type(node.stmts)
	if branch_type == 'int_literal' {
		branch_type = 'int'
	}
	mut else_type := ''
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		else_type = g.get_if_expr_type(&else_if)
	} else if node.else_expr !is ast.EmptyExpr {
		if nested := g.extract_if_expr(node.else_expr) {
			else_type = g.get_if_expr_type(&nested)
		} else {
			else_type = g.get_expr_type(node.else_expr)
			if else_type == '' || else_type == 'int' || else_type == 'void*'
				|| else_type == 'voidptr' {
				if raw := g.get_raw_type(node.else_expr) {
					raw_type := g.types_type_to_c(raw)
					if raw_type != '' && raw_type != 'void*' && raw_type != 'voidptr' {
						else_type = raw_type
					}
				}
			}
		}
	}
	if else_type == 'int_literal' {
		else_type = 'int'
	}
	if branch_type != '' && else_type != '' && branch_type == else_type {
		if branch_type != 'int' || env_type == '' || env_type == 'int' || env_type == 'void*'
			|| env_type == 'voidptr' {
			return branch_type
		}
	}
	if branch_type != '' && branch_type != 'int' {
		if env_type == '' || env_type == 'int' || branch_type.starts_with('${env_type}_T_') {
			return branch_type
		}
	}
	if env_type != '' {
		if g.cur_fn_ret_type.starts_with('${env_type}_T_') {
			return g.cur_fn_ret_type
		}
		return env_type
	}
	if else_type != '' {
		return else_type
	}
	return 'int'
}

// expr_produces_pointer checks if an expression produces a pointer value in C.
// More comprehensive than expr_is_pointer - handles casts, pointer arithmetic, etc.
// Used in gen_call_arg to avoid wrapping pointer expressions in compound literals.
