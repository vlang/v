module transformer

import v.pref
import v.ast

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

pub fn (t Transformer) stmt(mut node ast.Stmt) {
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
				expr := t.expr(field.expr)
				field = ast.ConstField{
					...(*field)
					expr: expr
				}
			}
		}
		ast.DeferStmt {}
		ast.EnumDecl {}
		ast.ExprStmt {
			expr := t.expr(node.expr)
			node = &ast.ExprStmt{
				...node
				expr: expr
			}
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

pub fn (t Transformer) expr(node ast.Expr) ast.Expr {
	match node {
		ast.InfixExpr { return t.infix_expr(node) }
		else { return node }
	}
}

pub fn (t Transformer) infix_expr(original ast.InfixExpr) ast.Expr {
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
