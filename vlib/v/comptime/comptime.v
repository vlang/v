module comptime

import v.pref
import v.ast

pub struct Comptime {
	pref &pref.Preferences
pub mut:
	table &ast.Table = unsafe { nil }
	file  &ast.File  = unsafe { nil }
}

pub fn new_comptime(pref_ &pref.Preferences) &Comptime {
	return &Comptime{
		pref: pref_
	}
}

pub fn new_comptime_with_table(table &ast.Table, pref_ &pref.Preferences) &Comptime {
	mut c := new_comptime(pref_)
	c.table = table
	return c
}

pub fn (mut c Comptime) solve_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
		c.solve(mut file)
	}
}

pub fn (mut c Comptime) solve(mut ast_file ast.File) {
	c.file = ast_file
	for mut stmt in ast_file.stmts {
		stmt = c.stmt(mut stmt)
	}
}

pub fn (mut c Comptime) stmt(mut node ast.Stmt) ast.Stmt {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.DebuggerStmt {}
		ast.AssertStmt {
			node.expr = c.expr(mut node.expr)
		}
		ast.AssignStmt {
			for mut right in node.right {
				right = c.expr(mut right)
			}
			for mut left in node.left {
				left = c.expr(mut left)
			}
		}
		ast.Block {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.BranchStmt {}
		ast.ComptimeFor {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.ConstDecl {
			for mut field in node.fields {
				field.expr = c.expr(mut field.expr)
			}
		}
		ast.DeferStmt {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.EnumDecl {
			for mut field in node.fields {
				if field.has_expr {
					field.expr = c.expr(mut field.expr)
				}
			}
		}
		ast.ExprStmt {
			node.expr = c.expr(mut node.expr)
		}
		ast.FnDecl {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.ForCStmt {
			if node.has_init && !node.is_multi {
				node.init = c.stmt(mut node.init)
			}

			if node.has_cond {
				node.cond = c.expr(mut node.cond)
			}

			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}

			if node.has_inc && !node.is_multi {
				node.inc = c.stmt(mut node.inc)
			}
		}
		ast.ForInStmt {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.ForStmt {
			node.cond = c.expr(mut node.cond)
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.GlobalDecl {
			for mut field in node.fields {
				field.expr = c.expr(mut field.expr)
			}
		}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {
			for mut cond in node.ct_conds {
				cond = c.expr(mut cond)
			}
		}
		ast.Import {}
		ast.InterfaceDecl {
			for mut field in node.fields {
				field.default_expr = c.expr(mut field.default_expr)
			}
		}
		ast.Module {}
		ast.Return {
			for mut expr in node.exprs {
				expr = c.expr(mut expr)
			}
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {}
		ast.StructDecl {
			for mut field in node.fields {
				field.default_expr = c.expr(mut field.default_expr)
			}
		}
		ast.TypeDecl {}
	}
	return node
}

pub fn (mut c Comptime) expr(mut node ast.Expr) ast.Expr {
	match mut node {
		ast.AnonFn {
			node.decl = c.stmt(mut node.decl) as ast.FnDecl
		}
		ast.ArrayDecompose {
			node.expr = c.expr(mut node.expr)
		}
		ast.ArrayInit {
			for mut expr in node.exprs {
				expr = c.expr(mut expr)
			}
			if node.has_len {
				node.len_expr = c.expr(mut node.len_expr)
			}
			if node.has_cap {
				node.cap_expr = c.expr(mut node.cap_expr)
			}
			if node.has_init {
				node.init_expr = c.expr(mut node.init_expr)
			}
		}
		ast.AsCast {
			node.expr = c.expr(mut node.expr)
		}
		ast.CTempVar {
			node.orig = c.expr(mut node.orig)
		}
		ast.CallExpr {
			node.left = c.expr(mut node.left)
			for mut arg in node.args {
				arg.expr = c.expr(mut arg.expr)
			}
			node.or_block = c.expr(mut node.or_block) as ast.OrExpr
		}
		ast.CastExpr {
			node.arg = c.expr(mut node.arg)
			node.expr = c.expr(mut node.expr)
		}
		ast.ChanInit {
			node.cap_expr = c.expr(mut node.cap_expr)
		}
		ast.ComptimeCall {
			for mut arg in node.args {
				arg.expr = c.expr(mut arg.expr)
			}
		}
		ast.ComptimeSelector {
			node.left = c.expr(mut node.left)
			node.field_expr = c.expr(mut node.field_expr)
		}
		ast.ConcatExpr {
			for mut val in node.vals {
				val = c.expr(mut val)
			}
		}
		ast.DumpExpr {
			node.expr = c.expr(mut node.expr)
		}
		ast.GoExpr {
			node.call_expr = c.expr(mut node.call_expr) as ast.CallExpr
		}
		ast.IfExpr {
			for mut branch in node.branches {
				branch.cond = c.expr(mut branch.cond)
				for mut stmt in branch.stmts {
					stmt = c.stmt(mut stmt)
				}
			}
			// where we place the result of the if when a := if ...
			node.left = c.expr(mut node.left)
		}
		ast.IfGuardExpr {
			node.expr = c.expr(mut node.expr)
		}
		ast.IndexExpr {
			node.left = c.expr(mut node.left)
			node.index = c.expr(mut node.index)
			node.or_expr = c.expr(mut node.or_expr) as ast.OrExpr
		}
		ast.InfixExpr {
			node.left = c.expr(mut node.left)
			node.right = c.expr(mut node.right)
		}
		ast.IsRefType {
			node.expr = c.expr(mut node.expr)
		}
		ast.Likely {
			node.expr = c.expr(mut node.expr)
		}
		ast.LockExpr {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
			for mut locked in node.lockeds {
				locked = c.expr(mut locked)
			}
		}
		ast.MapInit {
			for mut key in node.keys {
				key = c.expr(mut key)
			}
			for mut val in node.vals {
				val = c.expr(mut val)
			}
		}
		ast.MatchExpr {
			node.cond = c.expr(mut node.cond)
			for mut branch in node.branches {
				for mut expr in branch.exprs {
					expr = c.expr(mut expr)
				}
				for mut stmt in branch.stmts {
					stmt = c.stmt(mut stmt)
				}
			}
		}
		ast.OrExpr {
			for mut stmt in node.stmts {
				stmt = c.stmt(mut stmt)
			}
		}
		ast.ParExpr {
			node.expr = c.expr(mut node.expr)
		}
		ast.PostfixExpr {
			node.expr = c.expr(mut node.expr)
		}
		ast.PrefixExpr {
			node.right = c.expr(mut node.right)
			node.or_block = c.expr(mut node.or_block) as ast.OrExpr
		}
		ast.RangeExpr {
			node.low = c.expr(mut node.low)
			node.high = c.expr(mut node.high)
		}
		ast.SelectExpr {
			for mut branch in node.branches {
				branch.stmt = c.stmt(mut branch.stmt)
				for mut stmt in branch.stmts {
					stmt = c.stmt(mut stmt)
				}
			}
		}
		ast.SelectorExpr {
			node.expr = c.expr(mut node.expr)
		}
		ast.SizeOf {
			node.expr = c.expr(mut node.expr)
		}
		ast.SqlExpr {
			node.db_expr = c.expr(mut node.db_expr)
			if node.has_where {
				node.where_expr = c.expr(mut node.where_expr)
			}
			if node.has_order {
				node.order_expr = c.expr(mut node.order_expr)
			}
			if node.has_limit {
				node.limit_expr = c.expr(mut node.limit_expr)
			}
			if node.has_offset {
				node.offset_expr = c.expr(mut node.offset_expr)
			}
			for mut field in node.fields {
				field.default_expr = c.expr(mut field.default_expr)
			}
			for _, mut sub_struct in node.sub_structs {
				sub_struct = c.expr(mut sub_struct) as ast.SqlExpr
			}
		}
		ast.StringInterLiteral {
			for mut expr in node.exprs {
				expr = c.expr(mut expr)
			}
		}
		ast.StructInit {
			node.update_expr = c.expr(mut node.update_expr)
			for mut init_field in node.init_fields {
				init_field.expr = c.expr(mut init_field.expr)
			}
		}
		ast.UnsafeExpr {
			node.expr = c.expr(mut node.expr)
		}
		else {}
	}
	return node
}
