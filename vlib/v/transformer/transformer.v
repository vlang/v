module transformer

import v.pref
import v.ast
import v.util

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
		mut file := unsafe { ast_files[i] }
		t.transform(mut file)
	}
}

pub fn (t Transformer) transform(mut ast_file ast.File) {
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
		ast.ComptimeFor {}
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
			expr := node.expr
			node = &ast.ExprStmt{
				...node
				expr: match mut expr {
					ast.IfExpr {
						t.if_expr(mut expr)
					}
					ast.MatchExpr {
						t.match_expr(mut expr)
					}
					else {
						t.expr(expr)
					}
				}
			}
		}
		ast.FnDecl {
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
		}
		ast.ForCStmt {}
		ast.ForInStmt {}
		ast.ForStmt {
			node = &ast.ForStmt{
				...node
				cond: t.expr(node.cond)
			}
			if node.cond is ast.BoolLiteral && !(node.cond as ast.BoolLiteral).val { // for false { ... } should be eleminated
				node = &ast.EmptyStmt{}
			}
		}
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
	match mut node {
		ast.InfixExpr {
			return t.infix_expr(node)
		}
		ast.IndexExpr {
			return ast.IndexExpr{
				...node
				index: t.expr(node.index)
			}
		}
		ast.IfExpr {
			for mut branch in node.branches {
				branch = ast.IfBranch{
					...(*branch)
					cond: t.expr(branch.cond)
				}
				for i, mut stmt in branch.stmts {
					t.stmt(mut stmt)

					if i == branch.stmts.len - 1 {
						if stmt is ast.ExprStmt {
							expr := (stmt as ast.ExprStmt).expr

							match expr {
								ast.IfExpr {
									if expr.branches.len == 1 {
										branch.stmts.pop()
										branch.stmts << expr.branches[0].stmts
										break
									}
								}
								ast.MatchExpr {
									if expr.branches.len == 1 {
										branch.stmts.pop()
										branch.stmts << expr.branches[0].stmts
										break
									}
								}
								else {}
							}
						}
					}
				}
			}
			return node
		}
		ast.MatchExpr {
			node = ast.MatchExpr{
				...node
				cond: t.expr(node.cond)
			}
			for mut branch in node.branches {
				for mut expr in branch.exprs {
					expr = t.expr(expr)
				}
				for i, mut stmt in branch.stmts {
					t.stmt(mut stmt)

					if i == branch.stmts.len - 1 {
						if stmt is ast.ExprStmt {
							expr := (stmt as ast.ExprStmt).expr

							match expr {
								ast.IfExpr {
									if expr.branches.len == 1 {
										branch.stmts.pop()
										branch.stmts << expr.branches[0].stmts
										break
									}
								}
								ast.MatchExpr {
									if expr.branches.len == 1 {
										branch.stmts.pop()
										branch.stmts << expr.branches[0].stmts
										break
									}
								}
								else {}
							}
						}
					}
				}
			}
			return node
		}
		else {
			return node
		}
	}
}

pub fn (t Transformer) if_expr(mut original ast.IfExpr) ast.Expr {
	mut stop_index, mut unreachable_branches := -1, []int{cap: original.branches.len}
	if original.is_comptime {
		return *original
	}
	for i, mut branch in original.branches {
		for mut stmt in branch.stmts {
			t.stmt(mut stmt)
		}
		cond := t.expr(branch.cond)
		branch = ast.IfBranch{
			...(*branch)
			cond: cond
		}
		if cond is ast.BoolLiteral {
			if cond.val { // eliminates remaining branches when reached first bool literal `true`
				stop_index = i
				break
			} else { // discard unreachable branch when reached bool literal `false`
				unreachable_branches << i
			}
		}
	}
	if stop_index != -1 {
		unreachable_branches = unreachable_branches.filter(it < stop_index)
		original.branches = original.branches[..stop_index + 1]
	}
	for unreachable_branches.len != 0 {
		original.branches.delete(unreachable_branches.pop())
	}
	if original.branches.len == 0 { // no remain branches to walk through
		return ast.EmptyExpr{}
	}
	if original.branches.len == 1 && original.branches[0].cond.type_name() == 'unknown v.ast.Expr' {
		original.branches[0] = &ast.IfBranch{
			...original.branches[0]
			cond: ast.BoolLiteral{
				val: true
			}
		}
	}
	return *original
}

pub fn (t Transformer) match_expr(mut original ast.MatchExpr) ast.Expr {
	cond, mut terminate := t.expr(original.cond), false
	original = ast.MatchExpr{
		...(*original)
		cond: cond
	}
	for mut branch in original.branches {
		if branch.is_else {
			continue
		}

		for mut stmt in branch.stmts {
			t.stmt(mut stmt)
		}

		for mut expr in branch.exprs {
			expr = t.expr(expr)

			match cond {
				ast.BoolLiteral {
					if expr is ast.BoolLiteral {
						if cond.val == (expr as ast.BoolLiteral).val {
							branch.exprs = [expr]
							original = ast.MatchExpr{
								...(*original)
								branches: [branch]
							}
							terminate = true
						}
					}
				}
				ast.IntegerLiteral {
					if expr is ast.IntegerLiteral {
						if cond.val.int() == (expr as ast.IntegerLiteral).val.int() {
							branch.exprs = [expr]
							original = ast.MatchExpr{
								...(*original)
								branches: [branch]
							}
							terminate = true
						}
					}
				}
				ast.FloatLiteral {
					if expr is ast.FloatLiteral {
						if cond.val.f32() == (expr as ast.FloatLiteral).val.f32() {
							branch.exprs = [expr]
							original = ast.MatchExpr{
								...(*original)
								branches: [branch]
							}
							terminate = true
						}
					}
				}
				ast.StringLiteral {
					if expr is ast.StringLiteral {
						if cond.val == (expr as ast.StringLiteral).val {
							branch.exprs = [expr]
							original = ast.MatchExpr{
								...(*original)
								branches: [branch]
							}
							terminate = true
						}
					}
				}
				else {}
			}
		}

		if terminate {
			break
		}
	}
	return *original
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
						.eq {
							return ast.BoolLiteral{
								val: left_node.val == right_node.val
							}
						}
						.ne {
							return ast.BoolLiteral{
								val: left_node.val != right_node.val
							}
						}
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
						.eq {
							return ast.BoolLiteral{
								val: left_node.val == right_node.val
							}
						}
						.ne {
							return ast.BoolLiteral{
								val: left_node.val != right_node.val
							}
						}
						.plus {
							return if t.pref.backend == .c { ast.Expr(ast.StringLiteral{
									val: util.smart_quote(left_node.val, left_node.is_raw) + util.smart_quote(right_node.val, right_node.is_raw)
									pos: pos
								}) } else { ast.Expr(node) }
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
						.eq {
							return ast.BoolLiteral{
								val: left_node.val == right_node.val
							}
						}
						.ne {
							return ast.BoolLiteral{
								val: left_node.val != right_node.val
							}
						}
						.gt {
							return ast.BoolLiteral{
								val: left_node.val > right_node.val
							}
						}
						.ge {
							return ast.BoolLiteral{
								val: left_node.val >= right_node.val
							}
						}
						.lt {
							return ast.BoolLiteral{
								val: left_node.val < right_node.val
							}
						}
						.le {
							return ast.BoolLiteral{
								val: left_node.val <= right_node.val
							}
						}
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
								val: (u32(left_val) << right_val).str()
								pos: pos
							}
						}
						.right_shift {
							return ast.IntegerLiteral{
								val: (left_val >> right_val).str()
								pos: pos
							}
						}
						.unsigned_right_shift {
							return ast.IntegerLiteral{
								val: (left_val >>> right_val).str()
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
		ast.FloatLiteral {
			match right_node {
				ast.FloatLiteral {
					left_val := left_node.val.f32()
					right_val := right_node.val.f32()
					match node.op {
						.eq {
							return ast.BoolLiteral{
								val: left_node.val == right_node.val
							}
						}
						.ne {
							return ast.BoolLiteral{
								val: left_node.val != right_node.val
							}
						}
						.gt {
							return ast.BoolLiteral{
								val: left_node.val > right_node.val
							}
						}
						.ge {
							return ast.BoolLiteral{
								val: left_node.val >= right_node.val
							}
						}
						.lt {
							return ast.BoolLiteral{
								val: left_node.val < right_node.val
							}
						}
						.le {
							return ast.BoolLiteral{
								val: left_node.val <= right_node.val
							}
						}
						.plus {
							return ast.FloatLiteral{
								val: (left_val + right_val).str()
								pos: pos
							}
						}
						.mul {
							return ast.FloatLiteral{
								val: (left_val * right_val).str()
								pos: pos
							}
						}
						.minus {
							return ast.FloatLiteral{
								val: (left_val - right_val).str()
								pos: pos
							}
						}
						.div {
							return ast.FloatLiteral{
								val: (left_val / right_val).str()
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
