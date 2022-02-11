module transformer

import v.pref
import v.ast
import v.util

pub struct Transformer {
	pref &pref.Preferences
pub mut:
	index &IndexState
	table &ast.Table = 0
mut:
	is_assert bool
}

pub fn new_transformer(pref &pref.Preferences) &Transformer {
	return &Transformer{
		pref: pref
		index: &IndexState{
			saved_key_vals: [][]KeyVal{cap: 1000}
			saved_disabled: []bool{cap: 1000}
		}
	}
}

pub fn new_transformer_with_table(table &ast.Table, pref &pref.Preferences) &Transformer {
	mut transformer := new_transformer(pref)
	transformer.table = table
	return transformer
}

pub fn (mut t Transformer) transform_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
		t.transform(mut file)
	}
}

pub fn (mut t Transformer) transform(mut ast_file ast.File) {
	for mut stmt in ast_file.stmts {
		stmt = t.stmt(mut stmt)
	}
}

pub fn (mut t Transformer) find_new_array_len(node ast.AssignStmt) {
	if !t.pref.is_prod {
		return
	}
	// looking for, array := []type{len:int}
	mut right := node.right[0]
	if mut right is ast.ArrayInit {
		mut left := node.left[0]
		if mut left is ast.Ident {
			// we can not analyse mut array
			if left.is_mut {
				t.index.safe_access(left.name, -2)
				return
			}
			// as we do not need to check any value under the setup len
			if !right.has_len {
				t.index.safe_access(left.name, -1)
				return
			}

			mut len := int(0)

			value := right.len_expr
			if value is ast.IntegerLiteral {
				len = value.val.int() + 1
			}

			t.index.safe_access(left.name, len)
		}
	}
}

pub fn (mut t Transformer) find_new_range(node ast.AssignStmt) {
	if !t.pref.is_prod {
		return
	}
	// looking for, array := []type{len:int}
	mut right := node.right[0]
	if mut right is ast.IndexExpr {
		mut left := node.left[0]
		if mut left is ast.Ident {
			// we can not analyse mut array
			if left.is_mut {
				t.index.safe_access(left.name, -2)
				return
			}
			index := right.index
			if index is ast.RangeExpr {
				range_low := index.low
				if range_low is ast.IntegerLiteral {
					sub_left := right.left
					if sub_left is ast.Ident {
						safe := t.index.safe_offset(sub_left.name)
						low := range_low.val.int()
						if safe >= low {
							t.index.safe_access(left.name, safe - low)
						}
					}
				}
			}
		}
	}
}

pub fn (mut t Transformer) find_mut_self_assign(node ast.AssignStmt) {
	if !t.pref.is_prod {
		return
	}
	// even if mutable we can be sure than `a[1] = a[2] is safe
}

pub fn (mut t Transformer) check_safe_array(mut node ast.IndexExpr) {
	if !t.pref.is_prod {
		return
	}
	if !node.is_array {
		return
	}
	index := node.index
	name := node.left
	match index {
		ast.IntegerLiteral {
			is_direct := t.index.safe_access(name.str(), index.val.int())
			node.is_direct = is_direct
		}
		ast.RangeExpr {
			if index.has_high {
				high := index.high
				if high is ast.IntegerLiteral {
					t.index.safe_access(name.str(), high.val.int())
					return
				}
			}
			if index.has_low {
				low := index.low
				if low is ast.IntegerLiteral {
					t.index.safe_access(name.str(), low.val.int())
					return
				}
			}
		}
		ast.CastExpr {
			// do not deal with weird casting
			if index.typname != 'int' {
				return
			}
			index_expr := index.expr
			if index_expr is ast.IntegerLiteral {
				val := index_expr.val
				node.is_direct = t.index.safe_access(name.str(), val.int())
			}
		}
		ast.EnumVal {
			debug_bounds_checking('? $name[.$index.val] safe?: no-idea (yet)!')
		}
		ast.Ident {
			// we may be able to track const value in simple cases
		}
		else {}
	}
}

pub fn (mut t Transformer) stmt(mut node ast.Stmt) ast.Stmt {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.AssertStmt {
			return t.assert_stmt(mut node)
		}
		ast.AssignStmt {
			t.find_new_array_len(node)
			t.find_new_range(node)
			t.find_mut_self_assign(node)
			for mut right in node.right {
				right = t.expr(mut right)
			}
			for mut left in node.left {
				left = t.expr(mut left)
			}
		}
		ast.Block {
			t.index.indent(false)
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.BranchStmt {
			// break or continue:
			// we can not rely on sequential scanning and need to cancel all index optimisation
			t.index.disabled = true
		}
		ast.ComptimeFor {
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
		}
		ast.ConstDecl {
			for mut field in node.fields {
				field.expr = t.expr(mut field.expr)
			}
		}
		ast.DeferStmt {
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
		}
		ast.EnumDecl {}
		ast.ExprStmt {
			// TODO: check if this can be handled in `t.expr`
			node.expr = match mut node.expr {
				ast.IfExpr {
					t.expr_stmt_if_expr(mut node.expr)
				}
				ast.MatchExpr {
					t.expr_stmt_match_expr(mut node.expr)
				}
				else {
					t.expr(mut node.expr)
				}
			}
		}
		ast.FnDecl {
			t.index.indent(true)
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.ForCStmt {
			return t.for_c_stmt(mut node)
		}
		ast.ForInStmt {
			// indexes access within the for itself are not optimised (yet)
			t.index.indent(false)
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.ForStmt {
			return t.for_stmt(mut node)
		}
		ast.GlobalDecl {
			for mut field in node.fields {
				field.expr = t.expr(mut field.expr)
			}
		}
		ast.GotoLabel {}
		ast.GotoStmt {
			// we can not rely on sequential scanning and need to cancel all index optimisation
			t.index.disabled = true
		}
		ast.HashStmt {
			for mut cond in node.ct_conds {
				cond = t.expr(mut cond)
			}
		}
		ast.Import {}
		ast.InterfaceDecl {
			return t.interface_decl(mut node)
		}
		ast.Module {}
		ast.Return {
			for mut expr in node.exprs {
				expr = t.expr(mut expr)
			}
		}
		ast.SqlStmt {}
		ast.StructDecl {
			for mut field in node.fields {
				field.default_expr = t.expr(mut field.default_expr)
			}
		}
		ast.TypeDecl {}
	}
	return node
}

pub fn (mut t Transformer) assert_stmt(mut node ast.AssertStmt) ast.Stmt {
	t.is_assert = true
	node.expr = t.expr(mut node.expr)
	if !t.pref.is_prod {
		return node
	}
	if mut node.expr is ast.InfixExpr {
		right := node.expr.right
		match right {
			ast.IntegerLiteral {
				left := node.expr.left
				if left is ast.SelectorExpr {
					len := right.val.int()
					if left.field_name == 'len' {
						match node.expr.op {
							.eq { // ==
								t.index.safe_access(left.expr.str(), len - 1)
							}
							.ge { // >=
								t.index.safe_access(left.expr.str(), len - 1)
							}
							.gt { // >
								t.index.safe_access(left.expr.str(), len)
							}
							else {}
						}
					}
				}
			}
			ast.SelectorExpr {
				left := node.expr.left
				if left is ast.IntegerLiteral {
					len := left.val.int()
					if right.field_name == 'len' {
						match node.expr.op {
							.eq { // ==
								t.index.safe_access(right.expr.str(), len - 1)
							}
							.le { // <=
								t.index.safe_access(right.expr.str(), len - 1)
							}
							.lt { // <
								t.index.safe_access(right.expr.str(), len)
							}
							else {}
						}
					}
				}
			}
			else {}
		}
	}

	t.is_assert = false
	return node
}

pub fn (mut t Transformer) expr_stmt_if_expr(mut node ast.IfExpr) ast.Expr {
	mut stop_index, mut unreachable_branches := -1, []int{cap: node.branches.len}
	if node.is_comptime {
		return node
	}
	for i, mut branch in node.branches {
		cond := t.expr(mut branch.cond)
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
		t.index.indent(false)
		for mut stmt in branch.stmts {
			stmt = t.stmt(mut stmt)
		}
		t.index.unindent()
	}
	if stop_index != -1 {
		unreachable_branches = unreachable_branches.filter(it < stop_index)
		node.branches = node.branches[..stop_index + 1]
	}
	for unreachable_branches.len != 0 {
		node.branches.delete(unreachable_branches.pop())
	}
	/*
	FIXME: optimization causes cgen error `g.expr(): unhandled EmptyExpr`
	if original.branches.len == 0 { // no remain branches to walk through
		return ast.EmptyExpr{}
	}*/
	if node.branches.len == 1 && node.branches[0].cond.type_name() == 'unknown v.ast.Expr' {
		node.branches[0].cond = ast.BoolLiteral{
			val: true
		}
	}
	return node
}

pub fn (mut t Transformer) expr_stmt_match_expr(mut node ast.MatchExpr) ast.Expr {
	mut terminate := false
	cond := t.expr(mut node.cond)
	node.cond = cond
	for mut branch in node.branches {
		if branch.is_else {
			t.index.indent(false)
			for mut stmt in branch.stmts {
				stmt = t.stmt(mut stmt)
			}
			t.index.unindent()
			continue
		}

		for mut expr in branch.exprs {
			expr = t.expr(mut expr)

			match mut cond {
				ast.BoolLiteral {
					if expr is ast.BoolLiteral {
						if cond.val == (expr as ast.BoolLiteral).val {
							branch.exprs = [expr]
							node.branches = [branch]
							terminate = true
						}
					}
				}
				ast.IntegerLiteral {
					if expr is ast.IntegerLiteral {
						if cond.val.int() == (expr as ast.IntegerLiteral).val.int() {
							branch.exprs = [expr]
							node.branches = [branch]
							terminate = true
						}
					}
				}
				ast.FloatLiteral {
					if expr is ast.FloatLiteral {
						if cond.val.f32() == (expr as ast.FloatLiteral).val.f32() {
							branch.exprs = [expr]
							node.branches = [branch]
							terminate = true
						}
					}
				}
				ast.StringLiteral {
					if expr is ast.StringLiteral {
						if cond.val == (expr as ast.StringLiteral).val {
							branch.exprs = [expr]
							node.branches = [branch]
							terminate = true
						}
					}
				}
				else {}
			}
		}

		t.index.indent(false)
		for mut stmt in branch.stmts {
			stmt = t.stmt(mut stmt)
		}
		t.index.unindent()

		if terminate {
			break
		}
	}
	return node
}

pub fn (mut t Transformer) for_c_stmt(mut node ast.ForCStmt) ast.Stmt {
	// TODO we do not optimise array access for multi init
	// for a,b := 0,1; a < 10; a,b = a+b, a {...}

	if node.has_init && !node.is_multi {
		node.init = t.stmt(mut node.init)
	}

	if node.has_cond {
		node.cond = t.expr(mut node.cond)
	}

	t.index.indent(false)
	for mut stmt in node.stmts {
		stmt = t.stmt(mut stmt)
	}
	t.index.unindent()

	if node.has_inc && !node.is_multi {
		node.inc = t.stmt(mut node.inc)
	}
	return node
}

pub fn (mut t Transformer) for_stmt(mut node ast.ForStmt) ast.Stmt {
	node.cond = t.expr(mut node.cond)
	match node.cond {
		ast.BoolLiteral {
			if !(node.cond as ast.BoolLiteral).val { // for false { ... } should be eleminated
				return ast.EmptyStmt{}
			}
		}
		else {
			if !node.is_inf {
				t.index.indent(false)
				for mut stmt in node.stmts {
					stmt = t.stmt(mut stmt)
				}
				t.index.unindent()
			}
		}
	}
	for mut stmt in node.stmts {
		stmt = t.stmt(mut stmt)
	}
	return node
}

pub fn (mut t Transformer) interface_decl(mut node ast.InterfaceDecl) ast.Stmt {
	for mut field in node.fields {
		field.default_expr = t.expr(mut field.default_expr)
	}

	return node
}

pub fn (mut t Transformer) expr(mut node ast.Expr) ast.Expr {
	match mut node {
		ast.AnonFn {
			node.decl = t.stmt(mut node.decl) as ast.FnDecl
		}
		ast.ArrayDecompose {
			node.expr = t.expr(mut node.expr)
		}
		ast.ArrayInit {
			for mut expr in node.exprs {
				expr = t.expr(mut expr)
			}
			node.len_expr = t.expr(mut node.len_expr)
			node.cap_expr = t.expr(mut node.cap_expr)
			node.default_expr = t.expr(mut node.default_expr)
		}
		ast.AsCast {
			node.expr = t.expr(mut node.expr)
		}
		ast.CTempVar {
			node.orig = t.expr(mut node.orig)
		}
		ast.CallExpr {
			node.left = t.expr(mut node.left)
			for mut arg in node.args {
				arg.expr = t.expr(mut arg.expr)
			}
			node.or_block = t.expr(mut node.or_block) as ast.OrExpr
		}
		ast.CastExpr {
			node.arg = t.expr(mut node.arg)
			node.expr = t.expr(mut node.expr)
		}
		ast.ChanInit {
			node.cap_expr = t.expr(mut node.cap_expr)
		}
		ast.ComptimeCall {
			for mut arg in node.args {
				arg.expr = t.expr(mut arg.expr)
			}
		}
		ast.ComptimeSelector {
			node.left = t.expr(mut node.left)
			node.field_expr = t.expr(mut node.field_expr)
		}
		ast.ConcatExpr {
			for mut val in node.vals {
				val = t.expr(mut val)
			}
		}
		ast.DumpExpr {
			node.expr = t.expr(mut node.expr)
		}
		ast.GoExpr {
			node.call_expr = t.expr(mut node.call_expr) as ast.CallExpr
		}
		ast.IfExpr {
			return t.if_expr(mut node)
		}
		ast.IfGuardExpr {
			node.expr = t.expr(mut node.expr)
		}
		ast.IndexExpr {
			t.check_safe_array(mut node)
			node.left = t.expr(mut node.left)
			node.index = t.expr(mut node.index)
			node.or_expr = t.expr(mut node.or_expr) as ast.OrExpr
		}
		ast.InfixExpr {
			return t.infix_expr(mut node)
		}
		ast.IsRefType {
			node.expr = t.expr(mut node.expr)
		}
		ast.Likely {
			node.expr = t.expr(mut node.expr)
		}
		ast.LockExpr {
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
			for mut locked in node.lockeds {
				locked = t.expr(mut locked)
			}
		}
		ast.MapInit {
			for mut key in node.keys {
				key = t.expr(mut key)
			}
			for mut val in node.vals {
				val = t.expr(mut val)
			}
		}
		ast.MatchExpr {
			return t.match_expr(mut node)
		}
		ast.OrExpr {
			for mut stmt in node.stmts {
				stmt = t.stmt(mut stmt)
			}
		}
		ast.ParExpr {
			node.expr = t.expr(mut node.expr)
		}
		ast.PostfixExpr {
			node.expr = t.expr(mut node.expr)
		}
		ast.PrefixExpr {
			node.right = t.expr(mut node.right)
			node.or_block = t.expr(mut node.or_block) as ast.OrExpr
		}
		ast.RangeExpr {
			node.low = t.expr(mut node.low)
			node.high = t.expr(mut node.high)
		}
		ast.SelectExpr {
			for mut branch in node.branches {
				branch.stmt = t.stmt(mut branch.stmt)
				for mut stmt in branch.stmts {
					stmt = t.stmt(mut stmt)
				}
			}
		}
		ast.SelectorExpr {
			node.expr = t.expr(mut node.expr)
		}
		ast.SizeOf {
			node.expr = t.expr(mut node.expr)
		}
		ast.SqlExpr {
			return t.sql_expr(mut node)
		}
		ast.StringInterLiteral {
			for mut expr in node.exprs {
				expr = t.expr(mut expr)
			}
		}
		ast.StructInit {
			node.update_expr = t.expr(mut node.update_expr)
			for mut field in node.fields {
				field.expr = t.expr(mut field.expr)
			}
			for mut embed in node.embeds {
				embed.expr = t.expr(mut embed.expr)
			}
		}
		ast.UnsafeExpr {
			node.expr = t.expr(mut node.expr)
		}
		else {}
	}
	return node
}

pub fn (mut t Transformer) call_expr(mut node ast.CallExpr) ast.Expr {
	for mut arg in node.args {
		arg.expr = t.expr(mut arg.expr)
	}
	return node
}

pub fn (mut t Transformer) infix_expr(mut node ast.InfixExpr) ast.Expr {
	node.left = t.expr(mut node.left)
	node.right = t.expr(mut node.right)

	mut pos := node.left.pos()
	pos.extend(node.pos)
	pos.extend(node.right.pos())

	if t.pref.is_debug || t.is_assert { // never optimize assert statements
		return node
	} else {
		match mut node.left {
			ast.BoolLiteral {
				match mut node.right {
					ast.BoolLiteral {
						match node.op {
							.eq {
								return ast.BoolLiteral{
									val: node.left.val == node.right.val
								}
							}
							.ne {
								return ast.BoolLiteral{
									val: node.left.val != node.right.val
								}
							}
							.and {
								return ast.BoolLiteral{
									val: node.left.val && node.right.val
								}
							}
							.logical_or {
								return ast.BoolLiteral{
									val: node.left.val || node.right.val
								}
							}
							else {}
						}
					}
					else {}
				}
			}
			ast.StringLiteral {
				match mut node.right {
					ast.StringLiteral {
						match node.op {
							.eq {
								return ast.BoolLiteral{
									val: node.left.val == node.right.val
								}
							}
							.ne {
								return ast.BoolLiteral{
									val: node.left.val != node.right.val
								}
							}
							.plus {
								return if t.pref.backend == .c { ast.Expr(ast.StringLiteral{
										val: util.smart_quote(node.left.val, node.left.is_raw) + util.smart_quote(node.right.val, node.right.is_raw)
										pos: pos
									}) } else { ast.Expr(node) }
							}
							else {}
						}
					}
					else {}
				}
			}
			ast.IntegerLiteral {
				match mut node.right {
					ast.IntegerLiteral {
						left_val := node.left.val.i64()
						right_val := node.right.val.i64()

						match node.op {
							.eq {
								return ast.BoolLiteral{
									val: left_val == right_val
								}
							}
							.ne {
								return ast.BoolLiteral{
									val: left_val != right_val
								}
							}
							.gt {
								return ast.BoolLiteral{
									val: left_val > right_val
								}
							}
							.ge {
								return ast.BoolLiteral{
									val: left_val >= right_val
								}
							}
							.lt {
								return ast.BoolLiteral{
									val: left_val < right_val
								}
							}
							.le {
								return ast.BoolLiteral{
									val: left_val <= right_val
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
								// HACK: prevent folding of `min_i64` values in `math` module
								if left_val == -9223372036854775807 && right_val == 1 {
									return node
								}

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
							else {}
						}
					}
					else {}
				}
			}
			ast.FloatLiteral {
				match mut node.right {
					ast.FloatLiteral {
						left_val := node.left.val.f32()
						right_val := node.right.val.f32()
						match node.op {
							.eq {
								return ast.BoolLiteral{
									val: left_val == right_val
								}
							}
							.ne {
								return ast.BoolLiteral{
									val: left_val != right_val
								}
							}
							.gt {
								return ast.BoolLiteral{
									val: left_val > right_val
								}
							}
							.ge {
								return ast.BoolLiteral{
									val: left_val >= right_val
								}
							}
							.lt {
								return ast.BoolLiteral{
									val: left_val < right_val
								}
							}
							.le {
								return ast.BoolLiteral{
									val: left_val <= right_val
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
							else {}
						}
					}
					else {}
				}
			}
			else {}
		}
		return node
	}
}

pub fn (mut t Transformer) if_expr(mut node ast.IfExpr) ast.Expr {
	for mut branch in node.branches {
		branch.cond = t.expr(mut branch.cond)

		t.index.indent(false)
		for i, mut stmt in branch.stmts {
			stmt = t.stmt(mut stmt)

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
		t.index.unindent()
	}
	// where we place the result of the if when a := if ...
	node.left = t.expr(mut node.left)
	return node
}

pub fn (mut t Transformer) match_expr(mut node ast.MatchExpr) ast.Expr {
	node.cond = t.expr(mut node.cond)
	for mut branch in node.branches {
		for mut expr in branch.exprs {
			expr = t.expr(mut expr)
		}
		t.index.indent(false)
		for i, mut stmt in branch.stmts {
			stmt = t.stmt(mut stmt)

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
		t.index.unindent()
	}
	return node
}

pub fn (mut t Transformer) sql_expr(mut node ast.SqlExpr) ast.Expr {
	node.db_expr = t.expr(mut node.db_expr)
	if node.has_where {
		node.where_expr = t.expr(mut node.where_expr)
	}
	if node.has_order {
		node.order_expr = t.expr(mut node.order_expr)
	}
	if node.has_limit {
		node.limit_expr = t.expr(mut node.limit_expr)
	}
	if node.has_offset {
		node.offset_expr = t.expr(mut node.offset_expr)
	}
	for mut field in node.fields {
		field.default_expr = t.expr(mut field.default_expr)
	}
	for _, mut sub_struct in node.sub_structs {
		sub_struct = t.expr(mut sub_struct) as ast.SqlExpr
	}
	return node
}
