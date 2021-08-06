module optimizer

import v.pref
import v.ast

pub struct Optimizer {
	pref &pref.Preferences
}

pub fn new_optimizer(pref &pref.Preferences) &Optimizer {
	return &Optimizer{
		pref: pref
	}
}

pub fn (o Optimizer) optimize_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		file := unsafe { ast_files[i] }
		o.optimize(file)
	}
}

pub fn (o Optimizer) optimize(ast_file &ast.File) {
	for mut stmt in ast_file.stmts {
		o.stmt(mut stmt)
	}
}

fn (o Optimizer) stmt(mut node ast.Stmt) {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.AssertStmt {}
		ast.AssignStmt {
			for mut right in node.right {
				right = o.expr(right)
			}
		}
		ast.Block {
			for mut stmt in node.stmts {
				o.stmt(mut stmt)
			}
		}
		ast.BranchStmt {}
		ast.CompFor {}
		ast.ConstDecl {
			for mut field in node.fields {
				expr := o.expr(field.expr)
				field = ast.ConstField{
					...(*field)
					expr: expr
				}
			}
		}
		ast.DeferStmt {}
		ast.EnumDecl {}
		ast.ExprStmt {
			expr := o.expr(node.expr)
			node = &ast.ExprStmt{
				...node
				expr: expr
			}
		}
		ast.FnDecl {
			for mut stmt in node.stmts {
				o.stmt(mut stmt)
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
				expr = o.expr(expr)
			}
		}
		ast.SqlStmt {}
		ast.StructDecl {}
		ast.TypeDecl {}
	}
}

fn (o Optimizer) expr(node ast.Expr) ast.Expr {
	match node {
		ast.InfixExpr { return o.infix_expr(node) }
		ast.PrefixExpr { return o.prefix_expr(node) }
		else { return node }
	}
}

fn (o Optimizer) prefix_expr(original ast.PrefixExpr) ast.Expr {
	mut node := original
	node.right = o.expr(node.right)
	mut pos := node.pos
	pos.extend(node.right.position())
	right_node := node.right
	match right_node {
		ast.BoolLiteral {
			match node.op {
				.not {
					return ast.BoolLiteral{
						val: !right_node.val
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

fn (o Optimizer) infix_expr(original ast.InfixExpr) ast.Expr {
	mut node := original
	node.left = o.expr(node.left)
	node.right = o.expr(node.right)
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
							return ast.IntegerLiteral{
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
						.left_shift {
							return ast.IntegerLiteral{
								val: (left_val << right_val).str()
								pos: pos
							}
						}
						.right_shift {
							return ast.IntegerLiteral{
								val: (left_val >> right_val).str()
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
