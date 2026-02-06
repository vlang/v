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
				// Check for phi-node pattern: if-block modifies variables used later.
				// Transform: do case ... end  ->  let <Var_N> = case ... end in REST
				modified := g.core_find_if_modified_vars(if_expr)
				if modified.len > 0 {
					g.core_phi_if(if_expr, modified, stmts, idx + 1, write_indent)
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
			base_accs := g.core_find_loop_accumulators(stmt.stmts)
			has_index := !stmt.is_range && stmt.key_var.len > 0
				&& stmt.key_var != '_'
			if base_accs.len > 0 {
				// Loop with accumulators: bind result to accumulator var
				// let <NewAcc> = (foldl_expr) in REST
				if write_indent {
					g.write_indent_core()
				}
				// Capture init values BEFORE advancing SSA counter
				mut init_vals := []string{}
				for acc in base_accs {
					init_vals << g.core_var(acc)
				}
				// For indexed loops, add index as additional accumulator
				index_var := if has_index { stmt.key_var } else { '' }
				mut all_accs := base_accs.clone()
				if has_index {
					_ = g.core_var(stmt.key_var) // register index in var_map
					all_accs << stmt.key_var
					init_vals << '0'
				}
				if has_index {
					// foldl returns tuple - extract user accumulator
					tuple_temp := g.new_temp()
					new_var := g.next_core_var(base_accs[0])
					g.out.write_string('let <${tuple_temp}> =')
					g.out.writeln('')
					g.indent++
					g.write_indent_core()
					g.core_for_foldl(stmt, all_accs, init_vals, index_var)
					g.out.writeln('')
					g.indent--
					g.write_indent_core()
					g.out.write_string('in  let <${new_var}> =')
					g.out.writeln('')
					g.indent += 2
					g.write_indent_core()
					g.out.writeln("call 'erlang':'element'(1, ${tuple_temp})")
					g.indent -= 2
					g.indent++
					g.write_indent_core()
					g.out.write_string('in  ')
					g.indent++
					g.core_body_chain(stmts, idx + 1, false)
					g.indent -= 2
				} else {
					// Now advance SSA to get the new variable name
					new_var := g.next_core_var(all_accs[0])
					g.out.write_string('let <${new_var}> =')
					g.out.writeln('')
					g.indent++
					g.write_indent_core()
					g.core_for_foldl(stmt, all_accs, init_vals, '')
					g.out.writeln('')
					g.indent--
					g.write_indent_core()
					g.out.write_string('in  ')
					g.indent++
					g.core_body_chain(stmts, idx + 1, false)
					g.indent--
				}
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
			// Save old var name BEFORE advancing SSA counter.
			// The RHS must reference the old value, not the new binding.
			old_var_name := if left.name in g.var_map {
				g.var_map[left.name]
			} else {
				''
			}
			var_name := g.next_core_var(left.name)

			if i == 0 && write_indent {
				g.write_indent_core()
			}
			g.out.write_string('let <${var_name}> =')
			g.out.writeln('')
			g.indent++
			g.write_indent_core()
			// Temporarily restore old var name so RHS references old value
			if old_var_name.len > 0 {
				g.var_map[left.name] = old_var_name
			}
			g.core_expr(node.right[rhs_idx])
			// Restore new var name for subsequent code
			g.var_map[left.name] = var_name
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
				// Save old var name for RHS evaluation
				old_var_name := if ident.name in g.var_map {
					g.var_map[ident.name]
				} else {
					''
				}
				var_name := g.next_core_var(ident.name)
				if write_indent {
					g.write_indent_core()
				}
				g.out.write_string('let <${var_name}> =')
				g.out.writeln('')
				g.indent++
				g.write_indent_core()
				// RHS uses old var name
				if old_var_name.len > 0 {
					g.var_map[ident.name] = old_var_name
				}
				g.core_expr(stmt.right[0])
				g.var_map[ident.name] = var_name
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
// Checks top-level assignments AND assignments inside if-blocks (one level deep)
// to handle the common pattern of accumulator modification in conditionals.
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
		} else if stmt is ast.ExprStmt {
			// Check inside if-blocks for accumulator modifications
			if stmt.expr is ast.IfExpr {
				for branch in stmt.expr.branches {
					for bstmt in branch.stmts {
						if bstmt is ast.AssignStmt {
							for left in bstmt.left {
								if left is ast.Ident {
									if left.name in g.var_map && left.name !in accumulators {
										accumulators << left.name
									}
								}
							}
						}
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
fn (mut g CoreGen) core_for_foldl(node ast.ForInStmt, accumulators []string, init_vals []string, index_var string) {
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
	g.core_fold_body(node.stmts, accumulators, acc_in, acc_out, index_var)

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
// Uses var_map remapping so ALL expression types (method calls, index
// expressions, if-blocks, etc.) automatically resolve accumulators correctly.
// After executing the body, the accumulator's final var_map value is returned.
fn (mut g CoreGen) core_fold_body(stmts []ast.Stmt, accumulators []string, acc_in []string, acc_out []string, index_var string) {
	// Temporarily remap accumulator variables to acc_in parameter names.
	// Save original mappings so we can restore after.
	mut saved := map[string]string{}
	for i, acc in accumulators {
		if acc in g.var_map {
			saved[acc] = g.var_map[acc]
		}
		g.var_map[acc] = acc_in[i]
	}

	// Generate the body using the full statement chain mechanism.
	// This handles nested if-blocks, assignments, side effects etc.
	// We need to emit all statements as side effects, then return the accumulator.
	for s_idx, stmt in stmts {
		_ = s_idx
		match stmt {
			ast.AssignStmt {
				// Check if this assigns to an accumulator
				for j, left in stmt.left {
					if left is ast.Ident {
						acc_idx := accumulators.index(left.name)
						if acc_idx >= 0 {
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							// Save old name for RHS
							old_name := g.var_map[left.name]
							new_name := acc_out[acc_idx]
							g.write_indent_core()
							g.out.write_string('let <${new_name}> =')
							g.out.writeln('')
							g.indent++
							g.write_indent_core()
							g.core_expr(stmt.right[rhs_idx])
							g.out.writeln('')
							g.indent--
							g.write_indent_core()
							g.out.write_string('in  ')
							// Update var_map so subsequent refs use acc_out
							g.var_map[left.name] = new_name
							_ = old_name
						} else {
							// Non-accumulator assignment - regular let binding
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							var_name := g.next_core_var(left.name)
							g.write_indent_core()
							g.out.write_string('let <${var_name}> =')
							g.out.writeln('')
							g.indent++
							g.write_indent_core()
							g.core_expr(stmt.right[rhs_idx])
							g.out.writeln('')
							g.indent--
							g.write_indent_core()
							g.out.write_string('in  ')
						}
					}
				}
			}
			ast.ExprStmt {
				// Check if this is an if-block that modifies accumulators
				if stmt.expr is ast.IfExpr {
					modified := g.core_find_if_modified_vars(stmt.expr)
					// Filter to only accumulator-modified vars
					mut acc_modified := []string{}
					for m in modified {
						if accumulators.index(m) >= 0 {
							acc_modified << m
						}
					}
					if acc_modified.len > 0 {
						if acc_modified.len == 1 {
							// Single accumulator - simple phi-node
							mvar := acc_modified[0]
							acc_idx := accumulators.index(mvar)
							if acc_idx >= 0 {
								old_val := g.var_map[mvar]
								new_name := acc_out[acc_idx]
								g.write_indent_core()
								g.out.write_string('let <${new_name}> =')
								g.out.writeln('')
								g.indent++
								g.write_indent_core()
								g.core_phi_if_branches(stmt.expr.branches, 0, mvar, old_val)
								g.out.writeln('')
								g.indent--
								g.write_indent_core()
								g.out.write_string('in  ')
								g.var_map[mvar] = new_name
							}
						} else {
							// Multiple accumulators modified in same if-block.
							// Generate single case returning tuple, then extract.
							tuple_temp := g.new_temp()
							g.write_indent_core()
							g.out.write_string('let <${tuple_temp}> =')
							g.out.writeln('')
							g.indent++
							g.write_indent_core()
							g.core_phi_multi_if(stmt.expr, acc_modified, accumulators)
							g.out.writeln('')
							g.indent--
							g.write_indent_core()
							g.out.write_string('in  ')
							// Extract each element from tuple
							for j, mvar in acc_modified {
								acc_idx := accumulators.index(mvar)
								if acc_idx < 0 {
									continue
								}
								new_name := acc_out[acc_idx]
								g.write_indent_core()
								g.out.write_string('let <${new_name}> =')
								g.out.writeln('')
								g.indent++
								g.write_indent_core()
								g.out.writeln("call 'erlang':'element'(${j + 1}, ${tuple_temp})")
								g.indent--
								g.write_indent_core()
								g.out.write_string('in  ')
								g.var_map[mvar] = new_name
							}
						}
						continue
					}
				}
				// Regular side effect
				g.write_indent_core()
				g.out.write_string('do  ')
				g.core_expr(stmt.expr)
				g.out.writeln('')
				g.indent++
			}
			else {
				// Other statements as side effects
				g.write_indent_core()
				g.out.write_string('do  ')
				g.out.writeln("'ok'")
				g.indent++
			}
		}
	}

	// Return the accumulator(s) - use current var_map value
	// For index variables, auto-increment: call 'erlang':'+'(val, 1)
	if accumulators.len == 1 {
		if index_var.len > 0 && accumulators[0] == index_var {
			g.out.writeln("call 'erlang':'+'(${g.var_map[accumulators[0]]}, 1)")
		} else {
			g.out.writeln(g.var_map[accumulators[0]])
		}
	} else {
		mut vals := []string{}
		for acc in accumulators {
			if index_var.len > 0 && acc == index_var {
				vals << "call 'erlang':'+'(${g.var_map[acc]}, 1)"
			} else {
				vals << g.var_map[acc]
			}
		}
		g.out.writeln('{${vals.join(', ')}}')
	}

	// Unwind indent for side effects (do statements)
	// Count how many 'do' side effects we added
	for stmt in stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.IfExpr {
				modified := g.core_find_if_modified_vars(stmt.expr)
				mut has_acc := false
				for m in modified {
					if accumulators.index(m) >= 0 {
						has_acc = true
						break
					}
				}
				if has_acc {
					continue // phi-node, not a 'do'
				}
			}
			g.indent--
		}
	}

	// Restore original var_map
	for acc in accumulators {
		if acc in saved {
			g.var_map[acc] = saved[acc]
		} else {
			g.var_map.delete(acc)
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
	has_index := node.key_var.len > 0 && node.key_var != '_'
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()

	if has_index {
		// Indexed loop: for i, val in collection
		// Use lists:foldl with index counter:
		// let <Fun> = fun ({I, Val}, IdxAcc) -> do BODY call 'erlang':'+'(IdxAcc, 1)
		// in call 'lists':'foldl'(Fun, 0, List)
		idx_var := g.core_var(node.key_var)
		pair_temp := g.new_temp()
		g.out.write_string('let <${fun_temp}> =')
		g.out.writeln('')
		g.indent++
		g.write_indent_core()
		g.out.writeln('fun (${loop_var}, ${idx_var}) ->')
		g.indent++
		if node.stmts.len == 0 {
			g.write_indent_core()
			g.out.writeln("call 'erlang':'+'(${idx_var}, 1)")
		} else {
			// Generate body as side effects, then return idx + 1
			for stmt in node.stmts {
				g.write_indent_core()
				g.out.write_string('do  ')
				match stmt {
					ast.ExprStmt { g.core_expr(stmt.expr) }
					ast.AssignStmt {
						if stmt.right.len > 0 {
							g.core_expr(stmt.right[0])
						} else {
							g.out.write_string("'ok'")
						}
					}
					else { g.out.write_string("'ok'") }
				}
				g.out.writeln('')
				g.indent++
			}
			g.write_indent_core()
			g.out.writeln("call 'erlang':'+'(${idx_var}, 1)")
			for _ in node.stmts {
				g.indent--
			}
		}
		g.indent -= 2
		g.write_indent_core()
		_ = pair_temp
		g.out.write_string("in  call 'lists':'foldl'(${fun_temp}, 0, ")
		g.core_expr(node.cond)
		g.out.write_string(')')
	} else {
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

// Phi-node transformation for if-blocks that modify variables.
// Finds variables assigned in if-branches that exist in the current scope.
fn (mut g CoreGen) core_find_if_modified_vars(node ast.IfExpr) []string {
	mut modified := []string{}
	for branch in node.branches {
		for stmt in branch.stmts {
			if stmt is ast.AssignStmt {
				for left in stmt.left {
					if left is ast.Ident {
						if left.name in g.var_map && left.name !in modified {
							modified << left.name
						}
					}
				}
			}
		}
	}
	return modified
}

// core_phi_if generates a phi-node pattern for if-blocks that modify variables.
// Pattern: let <Var_N> = case COND of true -> NEW_VAL false -> OLD_VAL end in REST
fn (mut g CoreGen) core_phi_if(node ast.IfExpr, modified []string, stmts []ast.Stmt, next_idx int, write_indent bool) {
	// For each modified variable, generate a let binding around the case.
	// For simplicity, handle one variable at a time (most common case).
	// Multiple modified vars get nested let bindings.
	for midx, mvar in modified {
		old_val := g.core_var(mvar)
		new_var := g.next_core_var(mvar)

		// Temporarily restore old var mapping so the case condition
		// and branch body reference the OLD variable, not the new one.
		g.var_map[mvar] = old_val

		if midx == 0 && write_indent {
			g.write_indent_core()
		}
		g.out.write_string('let <${new_var}> =')
		g.out.writeln('')
		g.indent++
		g.write_indent_core()

		// Generate case expression that returns the new or old value
		g.core_phi_if_branches(node.branches, 0, mvar, old_val)

		// Restore new var mapping for subsequent code
		g.var_map[mvar] = new_var

		g.out.writeln('')
		g.indent--
		g.write_indent_core()
		g.out.write_string('in  ')
		g.indent++
	}

	// Continue with remaining statements
	g.core_body_chain(stmts, next_idx, false)

	for _ in modified {
		g.indent--
	}
}

// core_phi_if_branches generates case branches that return the modified variable value.
fn (mut g CoreGen) core_phi_if_branches(branches []ast.IfBranch, idx int, target_var string, old_val string) {
	if idx >= branches.len {
		g.write_core(old_val)
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if is_else {
		// Else branch: generate body and return target variable value
		g.core_phi_branch_body(branch, target_var, old_val)
	} else {
		g.write_core('case ')
		g.core_expr(branch.cond)
		g.write_core(" of <'true'> when 'true' -> ")
		g.core_phi_branch_body(branch, target_var, old_val)
		g.write_core(" <'false'> when 'true' -> ")
		if idx + 1 < branches.len {
			g.core_phi_if_branches(branches, idx + 1, target_var, old_val)
		} else {
			// No else branch - return old value
			g.write_core(old_val)
		}
		g.write_core(' end')
	}
}

// core_phi_branch_body generates a branch body that executes all statements
// and returns the target variable's value (or old value if not modified).
fn (mut g CoreGen) core_phi_branch_body(branch ast.IfBranch, target_var string, old_val string) {
	if branch.stmts.len == 0 {
		g.write_core(old_val)
		return
	}

	// Generate side-effect statements, then return the target variable.
	// Look for the last assignment to target_var - that's the return value.
	mut last_assign_idx := -1
	for i, stmt in branch.stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident && left.name == target_var {
					last_assign_idx = i
				}
			}
		}
	}

	if last_assign_idx < 0 {
		// Target not modified in this branch - generate side effects and return old
		if branch.stmts.len == 1 {
			first := branch.stmts[0]
			if first is ast.ExprStmt {
				g.write_core('do  ')
				g.core_expr(first.expr)
				g.write_core(' ')
				g.write_core(old_val)
			} else {
				g.write_core(old_val)
			}
		} else {
			g.write_core(old_val)
		}
		return
	}

	// Generate all statements up to and including the assignment.
	// Side-effect statements become `do`, assignments become `let`.
	for i, stmt in branch.stmts {
		if i > last_assign_idx {
			break
		}
		if stmt is ast.AssignStmt {
			for j, left in stmt.left {
				if left is ast.Ident && left.name == target_var {
					rhs_idx := if j < stmt.right.len { j } else { 0 }
					if i < last_assign_idx {
						// Not the final assignment - use do
						g.write_core('do  ')
						g.core_expr(stmt.right[rhs_idx])
						g.write_core(' ')
					} else {
						// Final assignment to target - this becomes the return value
						// Wrap preceding stmts as side effects
						g.core_expr(stmt.right[rhs_idx])
					}
				} else if left is ast.Ident {
					// Other variable assignment - generate let binding
					var_name := g.next_core_var(left.name)
					rhs_idx := if j < stmt.right.len { j } else { 0 }
					g.write_core('let <${var_name}> = ')
					g.core_expr(stmt.right[rhs_idx])
					g.write_core(' in ')
				}
			}
		} else if stmt is ast.ExprStmt {
			g.write_core('do  ')
			g.core_expr(stmt.expr)
			g.write_core(' ')
		}
	}
}

// core_phi_multi_if generates a single case expression that returns a tuple
// of multiple modified accumulator values. Used when an if-block modifies
// more than one accumulator to avoid cross-contamination of SSA variables.
fn (mut g CoreGen) core_phi_multi_if(if_expr ast.IfExpr, target_vars []string, accumulators []string) {
	g.core_phi_multi_branches(if_expr.branches, 0, target_vars)
}

fn (mut g CoreGen) core_phi_multi_branches(branches []ast.IfBranch, idx int, target_vars []string) {
	if idx >= branches.len {
		// No more branches - return old values as tuple
		g.write_core('{')
		for j, tv in target_vars {
			if j > 0 {
				g.write_core(', ')
			}
			g.write_core(g.core_var(tv))
		}
		g.write_core('}')
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if is_else {
		g.core_phi_multi_body(branch, target_vars)
	} else {
		g.write_core('case ')
		g.core_expr(branch.cond)
		g.write_core(" of <'true'> when 'true' -> ")
		g.core_phi_multi_body(branch, target_vars)
		g.write_core(" <'false'> when 'true' -> ")
		if idx + 1 < branches.len {
			g.core_phi_multi_branches(branches, idx + 1, target_vars)
		} else {
			// No else - return old values as tuple
			g.write_core('{')
			for j, tv in target_vars {
				if j > 0 {
					g.write_core(', ')
				}
				g.write_core(g.core_var(tv))
			}
			g.write_core('}')
		}
		g.write_core(' end')
	}
}

// core_phi_multi_body generates a tuple of values for a branch body,
// one value per target variable (RHS if assigned, old value if not).
fn (mut g CoreGen) core_phi_multi_body(branch ast.IfBranch, target_vars []string) {
	// Find last assignment index for each target variable
	mut assign_map := map[string]int{}
	for i, stmt in branch.stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident {
					for tv in target_vars {
						if left.name == tv {
							assign_map[tv] = i
						}
					}
				}
			}
		}
	}

	// Find the max assignment index (for generating side effects before it)
	mut max_assign := -1
	for tv in target_vars {
		if tv in assign_map && assign_map[tv] > max_assign {
			max_assign = assign_map[tv]
		}
	}

	// Generate side-effect statements (ExprStmt) before the last assignment
	for i, stmt in branch.stmts {
		if i > max_assign {
			break
		}
		if stmt is ast.ExprStmt {
			g.write_core('do  ')
			g.core_expr(stmt.expr)
			g.write_core(' ')
		}
	}

	// Generate tuple of values
	g.write_core('{')
	for j, tv in target_vars {
		if j > 0 {
			g.write_core(', ')
		}
		if tv in assign_map {
			// Find the assignment and emit its RHS
			stmt := branch.stmts[assign_map[tv]]
			if stmt is ast.AssignStmt {
				mut found := false
				for k, left in stmt.left {
					if left is ast.Ident && left.name == tv {
						rhs_idx := if k < stmt.right.len { k } else { 0 }
						g.core_expr(stmt.right[rhs_idx])
						found = true
					}
				}
				if !found {
					g.write_core(g.core_var(tv))
				}
			} else {
				g.write_core(g.core_var(tv))
			}
		} else {
			// Not modified in this branch - return old value
			g.write_core(g.core_var(tv))
		}
	}
	g.write_core('}')
}
