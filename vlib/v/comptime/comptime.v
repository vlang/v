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
	ast_file.stmts = c.stmts(mut ast_file.stmts)
}

pub fn (mut c Comptime) stmts(mut nodes []ast.Stmt) []ast.Stmt {
	for mut stmt in nodes {
		stmt = c.stmt(mut stmt)
	}
	return nodes
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
			node.right = c.exprs(mut node.right)
			node.left = c.exprs(mut node.left)
		}
		ast.Block {
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.BranchStmt {}
		ast.ComptimeFor {
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.ConstDecl {
			// node.fields = c.const_decl_fields(mut node.fields)
		}
		ast.DeferStmt {
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.EnumDecl {
			// node.fields = c.enum_decl_fields(mut node.fields)
		}
		ast.ExprStmt {
			res := c.expr_stmt(mut node.expr)
			match res {
				ast.Expr {
					node.expr = res
				}
				ast.Stmt {
					return res
				}
			}
		}
		ast.FnDecl {
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.ForCStmt {
			if node.has_init && !node.is_multi {
				node.init = c.stmt(mut node.init)
			}

			if node.has_cond {
				node.cond = c.expr(mut node.cond)
			}

			node.stmts = c.stmts(mut node.stmts)

			if node.has_inc && !node.is_multi {
				node.inc = c.stmt(mut node.inc)
			}
		}
		ast.ForInStmt {
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.ForStmt {
			node.cond = c.expr(mut node.cond)
			node.stmts = c.stmts(mut node.stmts)
		}
		ast.GlobalDecl {
			// node.fields = c.global_decl_fields(mut node.fields)
		}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {
			// node.ct.conds = c.hashstmt_ct_conds(mut node.ct_conds)
		}
		ast.Import {}
		ast.InterfaceDecl {
			// node.fields = c.interface_decl_fields(mut node.fields)
		}
		ast.Module {}
		ast.Return {
			node.exprs = c.exprs(mut node.exprs)
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {}
		ast.StructDecl {
			// node.fields = c.struct_decl_fields(mut node.fields)
		}
		ast.TypeDecl {}
	}
	return node
}

type StmtOrExpr = ast.Expr | ast.Stmt

pub fn (mut c Comptime) expr_stmt(mut node ast.Expr) StmtOrExpr {
	match mut node {
		ast.IfExpr {
			if node.is_comptime {
				if !node.is_expr && !node.has_else && node.branches.len == 1 {
					if node.branches[0].stmts.len == 0 {
						// empty ifdef; result of target OS != conditional => skip
						return ast.Stmt(ast.Block{
							pos:   node.pos
							scope: ast.empty_scope
						})
					}
					if !c.pref.output_cross_c {
						if node.branches[0].cond is ast.Ident {
							if c.pref.os == (pref.os_from_string(node.branches[0].cond.name) or {
								pref.OS._auto
							}) {
								// Same target OS as the conditional...
								// => skip the #if defined ... #endif wrapper
								// and just generate the branch statements:
								return ast.Stmt(ast.Block{
									stmts: node.branches[0].stmts
									scope: node.branches[0].scope
									pos:   node.pos
								})
							}
						}
					}
				}
			}
			return c.expr(mut node)
		}
		else {
			return c.expr(mut node)
		}
	}
	return node
}

pub fn (mut c Comptime) exprs(mut nodes []ast.Expr) []ast.Expr {
	for mut e in nodes {
		e = c.expr(mut e)
	}
	return nodes
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
			node.exprs = c.exprs(mut node.exprs)
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
				branch.stmts = c.stmts(mut branch.stmts)
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
			node.stmts = c.stmts(mut node.stmts)
			node.lockeds = c.exprs(mut node.lockeds)
		}
		ast.MapInit {
			node.keys = c.exprs(mut node.keys)
			node.vals = c.exprs(mut node.vals)
		}
		ast.MatchExpr {
			node.cond = c.expr(mut node.cond)
			for mut branch in node.branches {
				branch.exprs = c.exprs(mut branch.exprs)
				branch.stmts = c.stmts(mut branch.stmts)
			}
		}
		ast.OrExpr {
			node.stmts = c.stmts(mut node.stmts)
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
				branch.stmts = c.stmts(mut branch.stmts)
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
			node.exprs = c.exprs(mut node.exprs)
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
