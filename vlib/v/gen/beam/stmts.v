module beam

import v.ast
import strings

fn (mut g Gen) stmt(node ast.Stmt) {
	match node {
		ast.FnDecl { g.fn_decl(node) }
		ast.Return { g.return_stmt(node) }
		ast.ExprStmt { g.expr_stmt(node) }
		ast.AssignStmt { g.assign_stmt(node) }
		ast.ForInStmt { g.for_in_stmt(node) }
		ast.ComptimeFor { g.comptime_for(node) }
		ast.StructDecl {} // Skip struct declarations (handled via type info)
		ast.EnumDecl {} // Skip enum declarations (enums become atoms)
		ast.Module {} // Module declaration handled in gen_file
		ast.Import {} // Import statements handled differently
		else { g.writeln('% TODO: ${node}') }
	}
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	g.cur_fn = unsafe { &node }
	g.var_versions.clear()

	name := g.fn_name(node)
	g.writeln('')

	// Build parameter list
	// Note: In V's AST, node.params already includes the receiver as first element for methods
	// So we just iterate params directly, no need to add receiver separately
	mut params := []string{}
	for param in node.params {
		params << g.erl_var(param.name)
	}

	// Function head
	g.write_indent()
	// Simple names don't need quoting
	if g.needs_atom_quote(name) {
		g.write("'${name}'(${params.join(', ')}) ->")
	} else {
		g.write('${name}(${params.join(', ')}) ->')
	}
	g.out.writeln('')
	g.indent++

	// Generate body - handle early returns by restructuring
	if node.stmts.len == 0 {
		// Empty function body
		g.writeln('ok.')
	} else {
		g.gen_fn_body_with_early_returns(node.stmts)
	}

	g.indent--
	g.cur_fn = unsafe { nil }
}

// gen_fn_body_with_early_returns generates a function body, restructuring code
// so that statements after an `if` with a `return` in its true branch are
// placed in the `false` branch of the case expression.
//
// V code:
//   if n <= 1 { return n }
//   return fib(n - 1) + fib(n - 2)
//
// Becomes Erlang:
//   case N =< 1 of
//       true -> N;
//       false -> fib(N - 1) + fib(N - 2)
//   end.
fn (mut g Gen) gen_fn_body_with_early_returns(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.writeln('ok.')
		return
	}

	// Find the first if-with-early-return pattern
	for i, stmt in stmts {
		// Check if this is an ExprStmt containing an IfExpr
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				if_expr := stmt.expr
				// Check if the if has a return in its true branch and no else
				if g.is_early_return_if(if_expr) {
					// Generate case with remaining statements in false branch
					g.gen_early_return_case(if_expr, stmts[i + 1..])
					return
				}
			}
		}

		// Not an early-return if, generate normally
		g.is_last_stmt = i == stmts.len - 1
		g.gen_fn_stmt(stmt)
	}
}

// is_early_return_if checks if an if expression has:
// 1. A return statement in its true (first) branch
// 2. No else branch (or only a placeholder else)
fn (g Gen) is_early_return_if(node ast.IfExpr) bool {
	if node.branches.len == 0 {
		return false
	}

	// Check if first branch has a return
	first_branch := node.branches[0]
	has_return := g.branch_has_return(first_branch)

	// Check if there's no meaningful else branch
	// An if with just one branch (no else) is an early return candidate
	// An if with two branches where the second is a placeholder (empty else) is also a candidate
	no_else := node.branches.len == 1
	placeholder_else := node.branches.len == 2 && node.branches[1].stmts.len == 0

	return has_return && (no_else || placeholder_else)
}

// branch_has_return checks if a branch contains a return statement
fn (g Gen) branch_has_return(branch ast.IfBranch) bool {
	for stmt in branch.stmts {
		if stmt is ast.Return {
			return true
		}
	}
	return false
}

// gen_early_return_case generates a case expression where:
// - The true branch contains the early return value
// - The false branch contains the remaining statements (recursively handled)
fn (mut g Gen) gen_early_return_case(if_expr ast.IfExpr, remaining_stmts []ast.Stmt) {
	if if_expr.branches.len == 0 {
		return
	}

	branch := if_expr.branches[0]

	g.write_indent()
	g.write('case ')
	g.expr(branch.cond)
	g.write(' of')
	g.out.writeln('')
	g.indent++

	// True branch - the early return value
	g.write_indent()
	g.write('true -> ')
	g.gen_branch_return_value(branch)
	g.out.writeln(';')

	// False branch - the remaining statements
	g.write_indent()
	g.write('false -> ')

	if remaining_stmts.len == 0 {
		g.write('ok')
		g.out.writeln('')
	} else if remaining_stmts.len == 1 {
		// Single remaining statement - inline it
		stmt := remaining_stmts[0]
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.expr(stmt.exprs[0])
				} else {
					g.write('ok')
				}
				g.out.writeln('')
			}
			ast.ExprStmt {
				// Check if it's another early-return if
				if stmt.expr is ast.IfExpr {
					if g.is_early_return_if(stmt.expr) {
						g.out.writeln('')
						g.indent++
						g.gen_early_return_case(stmt.expr, []ast.Stmt{})
						g.indent--
						g.write_indent()
					} else {
						g.expr(stmt.expr)
						g.out.writeln('')
					}
				} else {
					g.expr(stmt.expr)
					g.out.writeln('')
				}
			}
			else {
				g.out.writeln('')
				g.indent++
				g.gen_fn_body_with_early_returns(remaining_stmts)
				g.indent--
				g.write_indent()
			}
		}
	} else {
		// Multiple remaining statements - check for nested early returns
		first := remaining_stmts[0]
		if first is ast.ExprStmt {
			if first.expr is ast.IfExpr {
				if g.is_early_return_if(first.expr) {
					// Nested early return - recurse
					g.out.writeln('')
					g.indent++
					g.gen_early_return_case(first.expr, remaining_stmts[1..])
					g.indent--
					g.write_indent()
				} else {
					// Regular if, use begin/end block
					g.out.writeln('begin')
					g.indent++
					g.gen_fn_body_with_early_returns(remaining_stmts)
					g.indent--
					g.write_indent()
					g.write('end')
					g.out.writeln('')
				}
			} else {
				// Not an if, use begin/end block
				g.out.writeln('begin')
				g.indent++
				g.gen_fn_body_with_early_returns(remaining_stmts)
				g.indent--
				g.write_indent()
				g.write('end')
				g.out.writeln('')
			}
		} else {
			// Use begin/end block for multiple statements
			g.out.writeln('begin')
			g.indent++
			g.gen_fn_body_with_early_returns(remaining_stmts)
			g.indent--
			g.write_indent()
			g.write('end')
			g.out.writeln('')
		}
	}

	g.indent--
	g.write_indent()
	g.writeln('end.')
}

// gen_branch_return_value extracts and generates the return value from a branch
fn (mut g Gen) gen_branch_return_value(branch ast.IfBranch) {
	for stmt in branch.stmts {
		if stmt is ast.Return {
			if stmt.exprs.len > 0 {
				g.expr(stmt.exprs[0])
				return
			}
		}
	}
	g.write('ok')
}

// gen_fn_stmt generates a statement within a function body
// handling commas vs periods and implicit return
fn (mut g Gen) gen_fn_stmt(stmt ast.Stmt) {
	is_last := g.is_last_stmt

	match stmt {
		ast.Return {
			// Explicit return
			if stmt.exprs.len > 0 {
				g.write_indent()
				g.expr(stmt.exprs[0])
				g.out.writeln('.')
			} else {
				g.writeln('ok.')
			}
		}
		ast.ExprStmt {
			// Check if expression is an if/match that has returns in branches
			// Use match instead of 'is' for proper sum type dispatch
			match stmt.expr {
				ast.IfExpr {
					g.write_indent()
					g.if_expr(stmt.expr)
					if is_last {
						g.out.writeln('.')
					} else {
						g.out.writeln(',')
					}
				}
				ast.MatchExpr {
					g.write_indent()
					g.match_expr(stmt.expr)
					if is_last {
						g.out.writeln('.')
					} else {
						g.out.writeln(',')
					}
				}
				else {
					g.write_indent()
					// Check if it's a call that returns a value we should use as implicit return
					if is_last {
						// Last statement - its value is the return value
						g.expr(stmt.expr)
						g.out.writeln(',')
						g.writeln('ok.')
					} else {
						g.expr(stmt.expr)
						g.out.writeln(',')
					}
				}
			}
		}
		ast.AssignStmt {
			g.gen_assign_stmt(stmt, is_last)
		}
		else {
			g.stmt(stmt)
		}
	}
}

fn (mut g Gen) return_stmt(node ast.Return) {
	if node.exprs.len > 0 {
		g.write_indent()
		g.expr(node.exprs[0])
		g.out.writeln('.')
	} else {
		g.writeln('ok.')
	}
}

fn (mut g Gen) expr_stmt(node ast.ExprStmt) {
	g.write_indent()
	g.expr(node.expr)
	g.out.writeln(',')
}

fn (mut g Gen) assign_stmt(node ast.AssignStmt) {
	g.gen_assign_stmt(node, false)
}

fn (mut g Gen) gen_assign_stmt(node ast.AssignStmt, is_last bool) {
	// SSA transform: each assignment creates new binding
	// Important: evaluate the RHS FIRST (using current versions),
	// then increment the version for the LHS

	// Handle tuple unpacking: x, y := func() where RHS is a single expression returning multiple values
	if node.left.len > 1 && node.right.len == 1 {
		// Evaluate RHS once, then destructure
		mut rhs := strings.new_builder(64)
		old_out := g.out
		g.out = rhs
		g.expr(node.right[0])
		rhs_str := g.out.str()
		g.out = old_out

		// Generate tuple destructuring: {Var1, Var2, ...} = RHS
		// But in Erlang, we need to bind to a temp and extract elements
		// For now, bind the first variable and ignore the rest (simplification)
		// TODO: proper tuple destructuring
		for i, left in node.left {
			if left is ast.Ident {
				if left.name == '_' {
					// Skip blank identifier
					continue
				}
				var_name := g.next_var(left.name)
				g.write_indent()
				if i == 0 {
					// First element - evaluate RHS and extract element 1
					g.write('${var_name} = element(${i + 1}, ${rhs_str})')
				} else {
					// Subsequent elements - just extract from the tuple
					g.write('${var_name} = element(${i + 1}, ${rhs_str})')
				}
				g.out.writeln(',')
			}
		}
		return
	}

	for i, left in node.left {
		if left is ast.Ident {
			// Capture the expression string BEFORE incrementing version
			// This ensures RHS uses the current version of variables
			mut rhs := strings.new_builder(64)
			old_out := g.out
			g.out = rhs
			g.expr(node.right[i])
			rhs_str := g.out.str()
			g.out = old_out

			// Now get the new variable name (this increments the version)
			var_name := g.next_var(left.name)
			g.write_indent()
			g.write('${var_name} = ')
			g.write(rhs_str)
			g.out.writeln(',')
		}
	}
}

// for_in_stmt handles V for loops
// for i in 0..n { body } -> lists:foldl with accumulator
// for item in arr { body } -> lists:foreach for side effects
fn (mut g Gen) for_in_stmt(node ast.ForInStmt) {
	// Check if this is a range iteration (0..n)
	// In V's AST, ranges use is_range flag on ForInStmt
	if node.is_range {
		g.for_range_stmt_v2(node)
		return
	}

	// Array/list iteration
	g.for_collection_stmt(node)
}

// for_range_stmt_v2 handles: for i in 0..n { body } using V's ForInStmt structure
// When is_range is true: cond is low bound, high is high bound
// Translates to: lists:foldl(fun(I, AccVars) -> ... end, InitVals, lists:seq(Low, High-1))
fn (mut g Gen) for_range_stmt_v2(node ast.ForInStmt) {
	// In V's AST, for ranges: cond = low, high = high bound
	// Identify accumulator variables: variables assigned in the loop body
	// that were declared before the loop
	accumulators := g.find_loop_accumulators(node.stmts)

	loop_var := g.erl_var(node.val_var)

	if accumulators.len == 0 {
		// No accumulators - use lists:foreach for side effects
		g.write_indent()
		g.write('lists:foreach(fun(${loop_var}) ->')
		g.out.writeln('')
		g.indent++

		// Generate loop body
		for stmt in node.stmts {
			g.gen_fn_stmt(stmt)
		}
		// Remove trailing comma and add 'ok'
		g.writeln('ok')

		g.indent--
		g.write_indent()
		g.write('end, lists:seq(')
		g.expr(node.cond)
		g.write(', ')
		g.expr(node.high)
		g.write(' - 1)),')
		g.out.writeln('')
	} else {
		// Has accumulators - use lists:foldl
		// Generate: AccVar = lists:foldl(fun(I, AccIn) -> ... AccOut end, InitVal, lists:seq(...))

		// Build accumulator tuple if multiple, or single var if one
		acc_vars_in := accumulators.map(fn (name string) string {
			return '${name.capitalize()}Acc'
		})
		acc_vars_out := accumulators.map(fn (name string) string {
			return '${name.capitalize()}Out'
		})

		// Get initial values (current SSA versions)
		mut init_vals := []string{}
		for acc in accumulators {
			init_vals << g.cur_var(acc)
		}

		// Get result variable names (next SSA versions)
		mut result_vars := []string{}
		for acc in accumulators {
			result_vars << g.next_var(acc)
		}

		// Generate the foldl
		g.write_indent()
		if accumulators.len == 1 {
			g.write('${result_vars[0]} = lists:foldl(fun(${loop_var}, ${acc_vars_in[0]}) ->')
		} else {
			g.write('{${result_vars.join(', ')}} = lists:foldl(fun(${loop_var}, {${acc_vars_in.join(', ')}}) ->')
		}
		g.out.writeln('')
		g.indent++

		// Generate loop body with accumulator tracking
		// Inside the fold, we need to map accumulator variables to the Acc names
		g.gen_fold_body(node.stmts, accumulators, acc_vars_in, acc_vars_out)

		// Return the accumulator(s)
		g.write_indent()
		if accumulators.len == 1 {
			g.write(acc_vars_out[0])
		} else {
			g.write('{${acc_vars_out.join(', ')}}')
		}
		g.out.writeln('')

		g.indent--
		g.write_indent()
		if accumulators.len == 1 {
			g.write('end, ${init_vals[0]}, lists:seq(')
		} else {
			g.write('end, {${init_vals.join(', ')}}, lists:seq(')
		}
		g.expr(node.cond)
		g.write(', ')
		g.expr(node.high)
		g.write(' - 1)),')
		g.out.writeln('')
	}
}

// for_collection_stmt handles: for item in arr { body }
fn (mut g Gen) for_collection_stmt(node ast.ForInStmt) {
	loop_var := g.erl_var(node.val_var)

	g.write_indent()
	g.write('lists:foreach(fun(${loop_var}) ->')
	g.out.writeln('')
	g.indent++

	for stmt in node.stmts {
		g.gen_fn_stmt(stmt)
	}
	g.writeln('ok')

	g.indent--
	g.write_indent()
	g.write('end, ')
	g.expr(node.cond)
	g.write('),')
	g.out.writeln('')
}

// find_loop_accumulators finds variables that are assigned in the loop body
fn (mut g Gen) find_loop_accumulators(stmts []ast.Stmt) []string {
	mut accumulators := []string{}
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident {
					// Check if this variable exists (was declared before the loop)
					if g.var_versions[left.name] >= 0 {
						if left.name !in accumulators {
							accumulators << left.name
						}
					}
				}
			}
		}
	}
	return accumulators
}

// gen_fold_body generates the body of a lists:foldl function
fn (mut g Gen) gen_fold_body(stmts []ast.Stmt, accumulators []string, acc_in []string, acc_out []string) {
	// Create a mapping from accumulator names to their Acc variable names
	// For assignments to accumulators, we output AccOut = expr using AccIn

	for stmt in stmts {
		if stmt is ast.AssignStmt {
			for i, left in stmt.left {
				if left is ast.Ident {
					// Check if this is an accumulator assignment
					acc_idx := accumulators.index(left.name)
					if acc_idx >= 0 {
						// This is an accumulator - generate AccOut = expr
						// where expr uses AccIn for the accumulator
						g.write_indent()
						g.write('${acc_out[acc_idx]} = ')

						// Generate RHS, substituting accumulator references
						// Handle tuple unpacking: if left.len > right.len, use right[0] for all
						rhs_idx := if i < stmt.right.len { i } else { 0 }
						g.gen_fold_expr(stmt.right[rhs_idx], accumulators, acc_in)
						g.out.writeln(',')
					} else {
						// Regular assignment
						g.gen_fn_stmt(stmt)
					}
				}
			}
		} else {
			g.gen_fn_stmt(stmt)
		}
	}
}

// gen_fold_expr generates an expression, substituting accumulator variable references
fn (mut g Gen) gen_fold_expr(expr ast.Expr, accumulators []string, acc_in []string) {
	match expr {
		ast.Ident {
			// Check if this is an accumulator reference
			acc_idx := accumulators.index(expr.name)
			if acc_idx >= 0 {
				g.write(acc_in[acc_idx])
			} else {
				g.expr(expr)
			}
		}
		ast.InfixExpr {
			g.gen_fold_expr(expr.left, accumulators, acc_in)
			g.write(' ${expr.op} ')
			g.gen_fold_expr(expr.right, accumulators, acc_in)
		}
		ast.ParExpr {
			g.write('(')
			g.gen_fold_expr(expr.expr, accumulators, acc_in)
			g.write(')')
		}
		else {
			// Fall back to regular expression generation
			g.expr(expr)
		}
	}
}
