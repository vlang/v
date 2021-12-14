module transformer

import v.pref
import v.ast
import v.util

struct KeyVal {
	key   string
	value int
}

[if debug_bounds_checking ?]
fn debug_bounds_checking(str string) {
	println(str)
}

// IndexState is used to track the index analysis performed when parsing the code
// `IndexExpr` nodes are annotated with `is_direct`, indicating that the array index can be safely directly accessed.

// The c_gen code check will handle this annotation and perform this direct memory access. The following cases are considered valid for this optimisation:
// 1. the array size is known and has a `len` larger than the index requested
// 2. the array was previously accessed with a higher value which would have reported the issue already
// 3. the array was created from a range expression a := range[10..13] and the offset'ed indexes are safe

// Current limitations:
//  * any function using break/continue or goto/label stopped from being optimised as soon as the relevant AST nodes are found as the code can not be ensured to be sequential
//  * `enum` and `const` indexes are not optimised (they could probably be looked up)
//  * for loops with multiple var in their init and/or inc are not analysed
//  * mut array are not analysed as their size can be reduced, but self-assignment in a single line

pub struct IndexState {
mut:
	// max_index has the biggest array index accessed for then named array
	// so if a[2] was set or read, it will be 2
	// A new array with no .len will recorded as -1 (accessing a[0] would be invalid)
	// the value -2 is used to indicate that the array should not be analysed
	// this is used for a mut array
	max_index map[string]int
	// We need to snapshot when entering `if` and `for` blocks and restore on exit
	// as the statements may not be run. This is managed by indent() & unindent().
	saved_disabled []bool
	saved_key_vals [][]KeyVal
pub mut:
	// on encountering goto/break/continue statements we stop any analysis
	// for the current function (as the code is not linear anymore)
	disabled bool
	level    int
}

// we are remembering the last array accessed and checking if the value is safe
// the node is updated with this information which can then be used by the code generators
fn (mut i IndexState) safe_access(key string, new int) bool {
	$if no_bounds_checking {
		return false
	}
	if i.disabled {
		return false
	}
	old := i.max_index[key] or {
		debug_bounds_checking('$i.level ${key}.len = $new')
		i.max_index[key] = new
		return false
	}
	if new > old {
		if old < -1 {
			debug_bounds_checking('$i.level $key[$new] unsafe (mut array)')
			return false
		}
		debug_bounds_checking('$i.level $key[$new] unsafe (index was $old)')
		i.max_index[key] = new
		return false
	}
	debug_bounds_checking('$i.level $key[$new] safe (index is $old)')
	return true
}

// safe_offset returns for a previvous array what was the highest
// offset we ever accessed for that identifier
fn (mut i IndexState) safe_offset(key string) int {
	$if no_bounds_checking {
		return -2
	}
	if i.disabled {
		return -2
	}
	return i.max_index[key] or { -1 }
}

// indent is used for when encountering new code blocks (if, for and functions)
// The code analysis needs to take into consideration blocks of code which
// may not run at runtime (if/for) and therefore even if a new maximum for an
// index access is found on an if branch it can not be used within the parent
// code. The same is true with for blocks. indent() snapshot the current state,
// to allow restoration with unindent()
// Also within a function, analysis must be `disabled` when goto or break are
// encountered as the code flow is then not lineear, and only restart when a
// new function analysis is started.
[if !no_bounds_checking]
fn (mut i IndexState) indent(is_function bool) {
	mut kvs := []KeyVal{cap: i.max_index.len}
	for k, v in i.max_index {
		kvs << KeyVal{k, v}
	}
	i.saved_disabled << i.disabled
	i.saved_key_vals << kvs
	if is_function {
		i.disabled = false
	}
	i.level += 1
}

// restoring the data as it was before the if/for/unsafe block
[if !no_bounds_checking]
fn (mut i IndexState) unindent() {
	i.level -= 1
	mut keys := []string{cap: i.max_index.len}
	for k, _ in i.max_index {
		keys << k
	}
	for k in keys {
		i.max_index.delete(k)
	}
	for saved in i.saved_key_vals.pop() {
		i.max_index[saved.key] = saved.value
	}
	i.disabled = i.saved_disabled.pop()
}

pub struct Transformer {
	pref &pref.Preferences
pub mut:
	index &IndexState
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

pub fn (mut t Transformer) transform_files(ast_files []&ast.File) {
	for i in 0 .. ast_files.len {
		mut file := unsafe { ast_files[i] }
		t.transform(mut file)
	}
}

pub fn (mut t Transformer) transform(mut ast_file ast.File) {
	for mut stmt in ast_file.stmts {
		t.stmt(mut stmt)
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

pub fn (mut t Transformer) find_assert_len(node ast.InfixExpr) {
	if !t.pref.is_prod {
		return
	}
	right := node.right
	match right {
		ast.IntegerLiteral {
			left := node.left
			if left is ast.SelectorExpr {
				len := right.val.int()
				if left.field_name == 'len' {
					match node.op {
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
			left := node.left
			if left is ast.IntegerLiteral {
				len := left.val.int()
				if right.field_name == 'len' {
					match node.op {
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
			node.is_direct = t.index.safe_access(name.str(), index.val.int())
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

pub fn (mut t Transformer) stmt(mut node ast.Stmt) {
	match mut node {
		ast.EmptyStmt {}
		ast.NodeError {}
		ast.AsmStmt {}
		ast.AssertStmt {
			expr := node.expr
			match expr {
				ast.InfixExpr {
					t.find_assert_len(expr)
				}
				else {
					t.expr(expr)
				}
			}
		}
		ast.AssignStmt {
			t.find_new_array_len(node)
			t.find_new_range(node)
			t.find_mut_self_assign(node)
			for mut right in node.right {
				right = t.expr(right)
			}
			for left in node.left {
				t.expr(left)
			}
		}
		ast.Block {
			t.index.indent(false)
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.BranchStmt {
			// break or continue:
			// we can not rely on sequential scanning and need to cancel all index optimisation
			t.index.disabled = true
		}
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
			t.index.indent(true)
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.ForCStmt {
			// TODO we do not optimise array access for multi init
			// for a,b := 0,1; a < 10; a,b = a+b, a {...}

			// https://github.com/vlang/v/issues/12782
			// if node.has_init && !node.is_multi {
			// 	mut init := node.init
			// 	t.stmt(mut init)
			// }

			if node.has_cond {
				t.expr(node.cond)
			}

			t.index.indent(false)
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
			t.index.unindent()

			// https://github.com/vlang/v/issues/12782
			// if node.has_inc && !node.is_multi {
			// 	mut inc := node.inc
			// 	t.stmt(mut inc)
			// }
		}
		ast.ForInStmt {
			// indexes access within the for itself are not optimised (yet)
			t.index.indent(false)
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
			t.index.unindent()
		}
		ast.ForStmt {
			cond_expr := t.expr(node.cond)

			node = &ast.ForStmt{
				...node
				cond: cond_expr
			}
			match node.cond {
				ast.BoolLiteral {
					if !(node.cond as ast.BoolLiteral).val { // for false { ... } should be eleminated
						node = &ast.EmptyStmt{}
					}
				}
				else {
					if !node.is_inf {
						t.index.indent(false)
						for mut stmt in node.stmts {
							t.stmt(mut stmt)
						}
						t.index.unindent()
					}
				}
			}
		}
		ast.GlobalDecl {}
		ast.GotoLabel {}
		ast.GotoStmt {
			// we can not rely on sequential scanning and need to cancel all index optimisation
			t.index.disabled = true
		}
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

pub fn (mut t Transformer) expr(node ast.Expr) ast.Expr {
	match mut node {
		ast.CallExpr {
			for arg in node.args {
				t.expr(arg.expr)
			}
			return node
		}
		ast.InfixExpr {
			return t.infix_expr(node)
		}
		ast.OrExpr {
			for mut stmt in node.stmts {
				t.stmt(mut stmt)
			}
			return node
		}
		ast.IndexExpr {
			t.check_safe_array(mut node)
			mut index := ast.IndexExpr{
				...node
				index: t.expr(node.index)
			}
			return index
		}
		ast.IfExpr {
			for mut branch in node.branches {
				branch = ast.IfBranch{
					...(*branch)
					cond: t.expr(branch.cond)
				}
				t.index.indent(false)
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
				t.index.unindent()
			}
			// where we place the result of the if when a := if ...
			t.expr(node.left)
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
				t.index.indent(false)
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
				t.index.unindent()
			}
			return node
		}
		ast.AnonFn {
			return node
		}
		ast.PostfixExpr {
			return node
		}
		ast.PrefixExpr {
			return node
		}
		ast.Likely {
			return node
		}
		else {
			return node
		}
	}
}

pub fn (mut t Transformer) if_expr(mut original ast.IfExpr) ast.Expr {
	mut stop_index, mut unreachable_branches := -1, []int{cap: original.branches.len}
	if original.is_comptime {
		return *original
	}
	for i, mut branch in original.branches {
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
		t.index.indent(false)
		for mut stmt in branch.stmts {
			t.stmt(mut stmt)
		}
		t.index.unindent()
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

pub fn (mut t Transformer) match_expr(mut original ast.MatchExpr) ast.Expr {
	cond, mut terminate := t.expr(original.cond), false
	original = ast.MatchExpr{
		...(*original)
		cond: cond
	}
	for mut branch in original.branches {
		if branch.is_else {
			t.index.indent(false)
			for mut stmt in branch.stmts {
				t.stmt(mut stmt)
			}
			t.index.unindent()
			continue
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

		t.index.indent(false)
		for mut stmt in branch.stmts {
			t.stmt(mut stmt)
		}
		t.index.unindent()

		if terminate {
			break
		}
	}
	return *original
}

pub fn (mut t Transformer) infix_expr(original ast.InfixExpr) ast.Expr {
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
