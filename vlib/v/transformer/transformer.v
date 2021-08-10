module transformer

import v.pref
import v.ast
import runtime

pub struct Transformer {
	pref &pref.Preferences
}

pub fn new_transformer(pref &pref.Preferences) &Transformer {
	return &Transformer{
		pref: pref
	}
}

pub fn (t Transformer) transform_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		file := unsafe { ast_files[i] }
		t.transform(file)
	}
}

pub fn (t Transformer) transform(ast_file &ast.File) {
	for mut stmt in ast_file.stmts {
		t.stmt(mut stmt)
	}
}

fn (t Transformer) stmt(mut node ast.Stmt) {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.AssertStmt {}
		ast.AssignStmt {
			for mut right in node.right {
				right = t.expr(right)
			}
		}
		ast.Block {
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
		}
		ast.BranchStmt {}
		ast.CompFor {}
		ast.ConstDecl {
			for mut field in node.fields {
				field.expr = t.expr(field.expr)
			}
		}
		ast.DeferStmt {}
		ast.EnumDecl {}
		ast.ExprStmt {
			expr := t.expr(node.expr)
			node.expr = expr
		}
		ast.FnDecl {
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
		}
		ast.ForCStmt {}
		ast.ForInStmt {}
		ast.ForStmt {}
		ast.GlobalDecl {}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {}
		ast.Import {}
		ast.InterfaceDecl {}
		ast.Module {}
		ast.Return {
			for mut expr in node.exprs {
				expr = t.expr(expr)
			}
		}
		ast.SqlStmt {}
		ast.StructDecl {}
		ast.TypeDecl {}
	}
}

fn (t Transformer) expr(node ast.Expr) ast.Expr {
	match node {
		ast.InfixExpr {
			return t.infix_expr(node)
		}
		ast.IfExpr {
			if node.is_comptime {
				return t.comp_if_expr(node)
			}
			return node
		}
		else {
			return node
		}
	}
}

fn (t Transformer) comp_if_expr(node ast.IfExpr) ast.Expr {
	for i, branch in node.branches {
		is_else_branch := i == node.branches.len - 1 && node.has_else
		if t.evaluate_comp_if_cond(branch.cond) || is_else_branch {
			// TODO: return ast.Block instead
			return ast.IfExpr{
				is_comptime: true
				is_expr: node.is_expr
				branches: [
					ast.IfBranch{
						cond: ast.BoolLiteral{
							val: true
						}
						stmts: branch.stmts
						scope: branch.scope
					},
				]
			}
		}
	}
	return ast.EmptyExpr{}
}

fn (t Transformer) evaluate_comp_if_cond(cond ast.Expr) bool {
	match cond {
		ast.BoolLiteral {
			return cond.val
		}
		ast.Ident {
			if os := pref.os_from_string(cond.name) {
				if os == t.pref.os {
					return true
				}
			}
			if pref.cc_from_string(cond.name) == t.pref.ccompiler_type {
				return true
			}
			if arch := pref.arch_from_string(cond.name) {
				if arch == t.pref.arch {
					return true
				}
			}
			if backend := pref.backend_from_string(cond.name) {
				if backend == t.pref.backend {
					return true
				}
			}
			if cond.name == 'big_endian' && runtime.is_big_endian() {
				return true
			}
			if cond.name == 'little_endian' && runtime.is_little_endian() {
				return true
			}
			// TODO: add bitness (x64 / x32)
			if cond.name == 'debug' && t.pref.is_debug {
				return true
			}
			if cond.name == 'prod' && t.pref.is_prod {
				return true
			}
			if cond.name == 'test' && t.pref.is_test {
				return true
			}
			return false
		}
		ast.PostfixExpr {
			if cond.op != .question {
				return false
			}
			if cond.expr is ast.Ident {
				name := cond.expr.name
				if name == 'gcboehm' && t.pref.gc_mode != .no_gc {
					return true
				}
				if name == 'glibc' {
					return false // currently not working
				}
				if name == 'prealloc' && t.pref.prealloc {
					return true
				}
				if name == 'freestanding' && (t.pref.is_bare || !t.pref.output_cross_c) {
					return true
				}
				if name in t.pref.compile_defines_all {
					return true
				}
			}
			return false
		}
		ast.PrefixExpr {
			if cond.op != .not {
				return false
			}
			return !t.evaluate_comp_if_cond(cond.right)
		}
		ast.ParExpr {
			return t.evaluate_comp_if_cond(cond.expr)
		}
		ast.InfixExpr {
			if cond.op == .and {
				return t.evaluate_comp_if_cond(cond.left) && t.evaluate_comp_if_cond(cond.right)
			}
			if cond.op == .logical_or {
				return t.evaluate_comp_if_cond(cond.left) || t.evaluate_comp_if_cond(cond.right)
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t Transformer) infix_expr(original ast.InfixExpr) ast.Expr {
	mut node := original
	node.left = t.expr(node.left)
	node.right = t.expr(node.right)
	mut pos := node.left.position()
	pos.extend(node.pos)
	pos.extend(node.right.position())
	left_node := node.left
	right_node := node.right
	match left_node {
		ast.BoolLiteral {
			match right_node {
				ast.BoolLiteral {
					match node.op {
						.and {
							return ast.BoolLiteral{
								val: left_node.val && right_node.val
							}
						}
						.logical_or {
							return ast.BoolLiteral{
								val: left_node.val || right_node.val
							}
						}
						else {
							return node
						}
					}
				}
				else {
					return node
				}
			}
		}
		ast.StringLiteral {
			match right_node {
				ast.StringLiteral {
					match node.op {
						.plus {
							return ast.StringLiteral{
								val: left_node.val + right_node.val
								pos: pos
							}
						}
						else {
							return node
						}
					}
				}
				else {
					return node
				}
			}
		}
		ast.IntegerLiteral {
			match right_node {
				ast.IntegerLiteral {
					left_val := left_node.val.int()
					right_val := right_node.val.int()
					match node.op {
						.plus {
							return ast.IntegerLiteral{
								val: (left_val + right_val).str()
								pos: pos
							}
						}
						.mul {
							return ast.IntegerLiteral{
								val: (left_val * right_val).str()
								pos: pos
							}
						}
						.minus {
							return ast.IntegerLiteral{
								val: (left_val - right_val).str()
								pos: pos
							}
						}
						.div {
							return ast.IntegerLiteral{
								val: (left_val / right_val).str()
								pos: pos
							}
						}
						.mod {
							return ast.IntegerLiteral{
								val: (left_val % right_val).str()
								pos: pos
							}
						}
						.xor {
							return ast.IntegerLiteral{
								val: (left_val ^ right_val).str()
								pos: pos
							}
						}
						.pipe {
							return ast.IntegerLiteral{
								val: (left_val | right_val).str()
								pos: pos
							}
						}
						.amp {
							return ast.IntegerLiteral{
								val: (left_val & right_val).str()
								pos: pos
							}
						}
						else {
							return node
						}
					}
				}
				else {
					return node
				}
			}
		}
		else {
			return node
		}
	}
}
