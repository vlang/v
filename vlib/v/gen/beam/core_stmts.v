module beam

import v.ast

// core_stmt dispatches top-level statements for Core Erlang generation.
// At module level, only function declarations produce output.
fn (mut g CoreGen) core_stmt(node ast.Stmt) {
	match node {
		ast.FnDecl { g.core_fn_decl(node) }
		ast.StructDecl {} // structs become maps
		ast.EnumDecl {} // enums become atoms
		ast.Module {} // handled in module header
		ast.Import {} // handled differently
		else {}
	}
}

fn (mut g CoreGen) core_fn_decl(node ast.FnDecl) {
	g.cur_fn = unsafe { &node }
	g.var_map.clear()
	g.temp_counter = 0

	name := g.core_fn_name(node)

	// Build parameter list
	mut params := []string{}
	for param in node.params {
		params << g.core_var(param.name)
	}

	// 'name'/arity =
	//     fun (Param1, Param2) ->
	//         body
	g.writeln_core("'${name}'/${params.len} =")
	g.indent++
	g.write_indent_core()
	g.out.write_string('fun (')
	g.out.write_string(params.join(', '))
	g.out.writeln(') ->')
	g.indent++

	// Generate body
	if node.stmts.len == 0 {
		g.write_indent_core()
		g.out.writeln("'ok'")
	} else {
		g.core_fn_body(node.stmts)
	}

	g.indent -= 2
	g.cur_fn = unsafe { nil }
}

// core_fn_body generates the function body from a list of statements.
// In Core Erlang, the body must be a single expression.
// Assignments become let...in, side effects become do.
fn (mut g CoreGen) core_fn_body(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.write_indent_core()
		g.out.writeln("'ok'")
		return
	}
	g.core_body_chain(stmts, 0, true)
}

// core_body_chain recursively generates a chain of let/do expressions.
// write_indent: whether to write leading indentation (false after 'in  ' prefix)
fn (mut g CoreGen) core_body_chain(stmts []ast.Stmt, idx int, write_indent bool) {
	if idx >= stmts.len {
		if write_indent {
			g.write_indent_core()
		}
		g.out.writeln("'ok'")
		return
	}

	stmt := stmts[idx]
	is_last := idx == stmts.len - 1

	if is_last {
		// Tail position - this is the return value
		g.core_tail_stmt(stmt, write_indent)
		return
	}

	// Non-tail: need let or do sequencing
	match stmt {
		ast.AssignStmt {
			g.core_let_chain(stmt, stmts, idx + 1, write_indent)
		}
		ast.ExprStmt {
			// Check for early-return-if pattern:
			// if cond { return val }  followed by remaining stmts
			if stmt.expr is ast.IfExpr {
				if_expr := stmt.expr
				if g.core_is_early_return_if(if_expr) {
					g.core_early_return_case(if_expr, stmts, idx + 1, write_indent)
					return
				}
			}
			// Regular side effect -> do EXPR REST
			if write_indent {
				g.write_indent_core()
			}
			g.out.write_string('do  ')
			g.core_expr(stmt.expr)
			g.out.writeln('')
			g.indent++
			g.core_body_chain(stmts, idx + 1, true)
			g.indent--
		}
		ast.Return {
			// Early return - generate expression (remaining stmts are dead code)
			g.core_tail_stmt(stmt, write_indent)
		}
		ast.ForInStmt {
			accumulators := g.core_find_loop_accumulators(stmt.stmts)
			if accumulators.len > 0 {
				// Loop with accumulators: bind result to accumulator var
				// let <NewAcc> = (foldl_expr) in REST
				if write_indent {
					g.write_indent_core()
				}
				// Capture init values BEFORE advancing SSA counter
				mut init_vals := []string{}
				for acc in accumulators {
					init_vals << g.core_var(acc)
				}
				// Now advance SSA to get the new variable name
				new_var := g.next_core_var(accumulators[0])
				g.out.write_string('let <${new_var}> =')
				g.out.writeln('')
				g.indent++
				g.write_indent_core()
				g.core_for_foldl(stmt, accumulators, init_vals)
				g.out.writeln('')
				g.indent--
				g.write_indent_core()
				g.out.write_string('in  ')
				g.indent++
				g.core_body_chain(stmts, idx + 1, false)
				g.indent--
			} else {
				// Side-effect only loop -> do (loop_expr) REST
				if write_indent {
					g.write_indent_core()
				}
				g.out.write_string('do  ')
				g.core_for_in(stmt)
				g.out.writeln('')
				g.indent++
				g.core_body_chain(stmts, idx + 1, true)
				g.indent--
			}
		}
		else {
			// Unknown statement - skip
			g.core_body_chain(stmts, idx + 1, write_indent)
		}
	}
}

// core_let_chain generates let bindings from an assignment statement,
// then continues with the rest of the body.
fn (mut g CoreGen) core_let_chain(node ast.AssignStmt, stmts []ast.Stmt, next_idx int, write_indent bool) {
	if node.left.len == 0 {
		g.core_body_chain(stmts, next_idx, write_indent)
		return
	}

	// Handle each left = right pair as a let binding
	mut let_count := 0
	for i, left in node.left {
		if left is ast.Ident {
			if left.name == '_' {
				// Discard: do EXPR REST
				if i == 0 && write_indent {
					g.write_indent_core()
				}
				rhs_idx := if i < node.right.len { i } else { 0 }
				g.out.write_string('do  ')
				g.core_expr(node.right[rhs_idx])
				g.out.writeln('')
				g.indent++
				if i == node.left.len - 1 {
					g.core_body_chain(stmts, next_idx, true)
				}
				g.indent--
				continue
			}

			rhs_idx := if i < node.right.len { i } else { 0 }
			var_name := g.next_core_var(left.name)

			if i == 0 && write_indent {
				g.write_indent_core()
			}
			g.out.write_string('let <${var_name}> =')
			g.out.writeln('')
			g.indent++
			g.write_indent_core()
			g.core_expr(node.right[rhs_idx])
			g.out.writeln('')
			g.indent--
			g.write_indent_core()
			g.out.write_string('in  ')
			g.indent++
			let_count++

			// Last binding in this assignment -> continue with rest of body
			if i == node.left.len - 1 {
				g.core_body_chain(stmts, next_idx, false)
			}
			// If more bindings, the loop continues and they chain
		} else {
			// Non-ident left side (index, selector, etc.) - treat as side effect
			if i == 0 && write_indent {
				g.write_indent_core()
			}
			rhs_idx := if i < node.right.len { i } else { 0 }
			g.out.write_string('do  ')
			g.core_expr(node.right[rhs_idx])
			g.out.writeln('')
			g.indent++
			if i == node.left.len - 1 {
				g.core_body_chain(stmts, next_idx, true)
			}
			g.indent--
		}
	}

	// Unwind indent for each let binding we added
	for _ in 0 .. let_count {
		g.indent--
	}
}

// core_tail_stmt generates a statement in tail (return) position.
fn (mut g CoreGen) core_tail_stmt(stmt ast.Stmt, write_indent bool) {
	match stmt {
		ast.Return {
			if write_indent {
				g.write_indent_core()
			}
			if stmt.exprs.len > 0 {
				g.core_expr(stmt.exprs[0])
			} else {
				g.out.write_string("'ok'")
			}
			g.out.writeln('')
		}
		ast.ExprStmt {
			if write_indent {
				g.write_indent_core()
			}
			g.core_expr(stmt.expr)
			g.out.writeln('')
		}
		ast.AssignStmt {
			// Assignment as last statement: let <V> = EXPR in V
			if stmt.left.len > 0 && stmt.left[0] is ast.Ident {
				ident := stmt.left[0] as ast.Ident
				var_name := g.next_core_var(ident.name)
				if write_indent {
					g.write_indent_core()
				}
				g.out.write_string('let <${var_name}> =')
				g.out.writeln('')
				g.indent++
				g.write_indent_core()
				g.core_expr(stmt.right[0])
				g.out.writeln('')
				g.indent--
				g.write_indent_core()
				g.out.writeln('in  ${var_name}')
			} else {
				if write_indent {
					g.write_indent_core()
				}
				g.out.writeln("'ok'")
			}
		}
		else {
			if write_indent {
				g.write_indent_core()
			}
			g.out.writeln("'ok'")
		}
	}
}

// core_for_in generates a for-in loop as an expression.
// In Core Erlang, lambdas must be bound to a variable first via let.
// Range loops with accumulators -> lists:foldl
// Range loops without -> lists:foreach
// Collection loops -> lists:foreach
fn (mut g CoreGen) core_for_in(node ast.ForInStmt) {
	// Note: accumulator loops are handled directly by core_body_chain
	// which calls core_for_foldl with pre-captured init values.
	// This function handles side-effect-only loops.
	if node.is_range {
		g.core_for_range_foreach(node)
	} else {
		g.core_for_collection_foreach(node)
	}
}

// core_find_loop_accumulators finds variables assigned in the loop body
// that were declared before the loop (exist in var_map).
fn (mut g CoreGen) core_find_loop_accumulators(stmts []ast.Stmt) []string {
	mut accumulators := []string{}
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident {
					if left.name in g.var_map && left.name !in accumulators {
						accumulators << left.name
					}
				}
			}
		}
	}
	return accumulators
}

// core_for_foldl generates a for loop with accumulators using lists:foldl.
// Pattern:
//   let <Fun> = fun (I, Acc) -> NewAcc
//   in  let <Seq> = call 'lists':'seq'(Low, High-1)
//       in  call 'lists':'foldl'(Fun, InitVal, Seq)
fn (mut g CoreGen) core_for_foldl(node ast.ForInStmt, accumulators []string, init_vals []string) {
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()
	seq_temp := g.new_temp()

	// Create accumulator input/output variable names for the lambda
	mut acc_in := []string{}
	mut acc_out := []string{}
	for acc in accumulators {
		acc_in << '${acc.capitalize()}Acc'
		g.temp_counter++
		acc_out << '${acc.capitalize()}Out_${g.temp_counter}'
	}

	// let <Fun> = fun (LoopVar, AccIn) -> ... AccOut
	g.out.write_string('let <${fun_temp}> =')
	g.out.writeln('')
	g.indent++
	g.write_indent_core()
	if accumulators.len == 1 {
		g.out.writeln('fun (${loop_var}, ${acc_in[0]}) ->')
		g.indent++
	} else {
		// Core Erlang doesn't support tuple patterns in fun args.
		// Use a single tuple param and destructure with element/2.
		acc_tuple := g.new_temp()
		g.out.writeln('fun (${loop_var}, ${acc_tuple}) ->')
		g.indent++
		for j, ain in acc_in {
			g.write_indent_core()
			g.out.write_string("let <${ain}> =")
			g.out.writeln('')
			g.indent++
			g.write_indent_core()
			g.out.writeln("call 'erlang':'element'(${j + 1}, ${acc_tuple})")
			g.indent--
			g.write_indent_core()
			g.out.write_string('in  ')
		}
		g.out.writeln('')
	}

	// Generate fold body: compute new accumulator values
	g.core_fold_body(node.stmts, accumulators, acc_in, acc_out)

	g.indent -= 2
	g.write_indent_core()
	g.out.write_string('in  ')

	// let <Seq> = call 'lists':'seq'(Low, High - 1)
	if node.is_range {
		g.out.write_string('let <${seq_temp}> =')
		g.out.writeln('')
		g.indent++
		g.write_indent_core()
		g.out.write_string("call 'lists':'seq'(")
		g.core_expr(node.cond)
		g.out.write_string(", call 'erlang':'-'(")
		g.core_expr(node.high)
		g.out.writeln(', 1))')
		g.indent--
		g.write_indent_core()
		g.out.write_string('in  ')
	}

	// call 'lists':'foldl'(Fun, InitVal, Seq)
	list_arg := if node.is_range { seq_temp } else { '' }
	if accumulators.len == 1 {
		g.out.write_string("call 'lists':'foldl'(${fun_temp}, ${init_vals[0]}, ")
	} else {
		g.out.write_string("call 'lists':'foldl'(${fun_temp}, {${init_vals.join(', ')}}, ")
	}
	if node.is_range {
		g.out.write_string('${list_arg})')
	} else {
		g.core_expr(node.cond)
		g.out.write_string(')')
	}

	// After the fold, update accumulator variables with the result
	// The fold returns the new accumulator value
}

// core_fold_body generates the body of a foldl lambda.
// Maps accumulator references to AccIn vars and produces AccOut.
fn (mut g CoreGen) core_fold_body(stmts []ast.Stmt, accumulators []string, acc_in []string, acc_out []string) {
	// For each assignment to an accumulator, generate let with AccIn substitution
	// For simplicity, handle the common pattern: accumulator = accumulator OP expr
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			for j, left in stmt.left {
				if left is ast.Ident {
					acc_idx := accumulators.index(left.name)
					if acc_idx >= 0 {
						rhs_idx := if j < stmt.right.len { j } else { 0 }
						g.write_indent_core()
						g.out.write_string('let <${acc_out[acc_idx]}> =')
						g.out.writeln('')
						g.indent++
						g.write_indent_core()
						g.core_fold_expr(stmt.right[rhs_idx], accumulators, acc_in)
						g.out.writeln('')
						g.indent--
						g.write_indent_core()
						g.out.write_string('in  ')
					}
				}
			}
		}
	}
	// Return the accumulator(s)
	if accumulators.len == 1 {
		g.out.writeln(acc_out[0])
	} else {
		g.out.writeln('{${acc_out.join(', ')}}')
	}
}

// core_fold_expr generates an expression, substituting accumulator variable refs
fn (mut g CoreGen) core_fold_expr(expr ast.Expr, accumulators []string, acc_in []string) {
	match expr {
		ast.Ident {
			acc_idx := accumulators.index(expr.name)
			if acc_idx >= 0 {
				g.write_core(acc_in[acc_idx])
			} else {
				g.core_expr(expr)
			}
		}
		ast.InfixExpr {
			// All operators become BIF calls with accumulator substitution
			op_str := core_op(expr.op)
			g.write_core("call 'erlang':'${op_str}'(")
			g.core_fold_expr(expr.left, accumulators, acc_in)
			g.write_core(', ')
			g.core_fold_expr(expr.right, accumulators, acc_in)
			g.write_core(')')
		}
		ast.ParExpr {
			g.core_fold_expr(expr.expr, accumulators, acc_in)
		}
		else {
			g.core_expr(expr)
		}
	}
}

fn (mut g CoreGen) core_for_range_foreach(node ast.ForInStmt) {
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()
	seq_temp := g.new_temp()

	// let <Fun> = fun (LoopVar) -> body
	g.out.write_string('let <${fun_temp}> =')
	g.out.writeln('')
	g.indent++
	g.write_indent_core()
	g.out.writeln('fun (${loop_var}) ->')
	g.indent++
	if node.stmts.len == 0 {
		g.write_indent_core()
		g.out.writeln("'ok'")
	} else {
		g.core_fn_body(node.stmts)
	}
	g.indent -= 2

	// in  let <Seq> = call 'lists':'seq'(Low, High-1)
	g.write_indent_core()
	g.out.write_string('in  let <${seq_temp}> =')
	g.out.writeln('')
	g.indent += 2
	g.write_indent_core()
	g.out.write_string("call 'lists':'seq'(")
	g.core_expr(node.cond)
	g.out.write_string(", call 'erlang':'-'(")
	g.core_expr(node.high)
	g.out.writeln(', 1))')
	g.indent -= 2
	g.indent++
	g.write_indent_core()
	g.out.write_string("in  call 'lists':'foreach'(${fun_temp}, ${seq_temp})")
	g.indent--
}

fn (mut g CoreGen) core_for_collection_foreach(node ast.ForInStmt) {
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()

	// let <Fun> = fun (LoopVar) -> body
	g.out.write_string('let <${fun_temp}> =')
	g.out.writeln('')
	g.indent++
	g.write_indent_core()
	g.out.writeln('fun (${loop_var}) ->')
	g.indent++
	if node.stmts.len == 0 {
		g.write_indent_core()
		g.out.writeln("'ok'")
	} else {
		g.core_fn_body(node.stmts)
	}
	g.indent -= 2
	g.write_indent_core()
	g.out.write_string("in  call 'lists':'foreach'(${fun_temp}, ")
	g.core_expr(node.cond)
	g.out.write_string(')')
}

// Early return handling: if cond { return val } rest...
// Becomes: case COND of <'true'> -> VAL <'false'> -> REST end

fn (g CoreGen) core_is_early_return_if(node ast.IfExpr) bool {
	if node.branches.len == 0 {
		return false
	}
	first_branch := node.branches[0]
	has_return := g.core_branch_has_return(first_branch)
	no_else := node.branches.len == 1
	placeholder_else := node.branches.len == 2 && node.branches[1].stmts.len == 0
	return has_return && (no_else || placeholder_else)
}

fn (g CoreGen) core_branch_has_return(branch ast.IfBranch) bool {
	for stmt in branch.stmts {
		if stmt is ast.Return {
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_early_return_case(if_expr ast.IfExpr, stmts []ast.Stmt, next_idx int, write_indent bool) {
	if if_expr.branches.len == 0 {
		return
	}
	branch := if_expr.branches[0]

	if write_indent {
		g.write_indent_core()
	}
	// case COND of
	//     <'true'> when 'true' -> RETURN_VAL
	//     <'false'> when 'true' -> REST
	// end
	g.out.write_string('case ')
	g.core_expr(branch.cond)
	g.out.writeln(' of')
	g.indent++

	// True branch - the early return value
	g.write_indent_core()
	g.out.write_string("<'true'> when 'true' -> ")
	g.core_branch_return_val(branch)
	g.out.writeln('')

	// False branch - the remaining statements
	g.write_indent_core()
	g.out.write_string("<'false'> when 'true' -> ")

	remaining := stmts[next_idx..]
	if remaining.len == 0 {
		g.out.writeln("'ok'")
	} else {
		g.out.writeln('')
		g.indent++
		// Check if first remaining is another early-return-if
		g.core_body_chain(remaining, 0, true)
		g.indent--
	}

	g.indent--
	g.write_indent_core()
	g.out.writeln('end')
}

fn (mut g CoreGen) core_branch_return_val(branch ast.IfBranch) {
	for stmt in branch.stmts {
		if stmt is ast.Return {
			if stmt.exprs.len > 0 {
				g.core_expr(stmt.exprs[0])
				return
			}
		}
	}
	g.write_core("'ok'")
}
