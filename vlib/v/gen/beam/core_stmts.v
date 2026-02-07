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
	// Skip methods on interface types — they are abstract.
	// See core_collect_fn_info for the rationale.
	if node.is_method {
		rec_type := node.receiver.typ
		type_sym := g.table.sym(rec_type)
		if type_sym.kind == .interface {
			return
		}
	}

	g.cur_fn = unsafe { &node }
	g.var_map.clear()
	g.temp_counter = 0

	name := g.core_fn_name(node)

	// Build parameter list
	mut params := []string{}
	for param in node.params {
		params << g.core_var(param.name)
	}

	if g.etf_mode {
		// ETF mode: {FnVar, {c_fun,[],[Params],Body}}
		if g.fn_count > 0 {
			g.fndef_sep()
		}
		g.fn_count++
		g.begin_fndef(name, params.len)
		g.begin_fun(params)
		if node.stmts.len == 0 {
			g.emit_atom('ok')
		} else {
			g.core_fn_body(node.stmts)
		}
		g.end_fun()
		g.end_fndef()
	} else {
		// Text mode: 'name'/arity = fun (Params) -> Body
		g.writeln_core("'${name}'/${params.len} =")
		g.indent++
		g.write_indent_core()
		g.out.write_string('fun (')
		g.out.write_string(params.join(', '))
		g.out.writeln(') ->')
		g.indent++

		if node.stmts.len == 0 {
			g.write_indent_core()
			g.out.writeln("'ok'")
		} else {
			g.core_fn_body(node.stmts)
		}

		g.indent -= 2
	}
	g.cur_fn = unsafe { nil }
}

// core_fn_body generates the function body from a list of statements.
// In Core Erlang, the body must be a single expression.
// Assignments become let...in, side effects become do.
fn (mut g CoreGen) core_fn_body(stmts []ast.Stmt) {
	if stmts.len == 0 {
		if g.etf_mode {
			g.emit_atom('ok')
		} else {
			g.write_indent_core()
			g.out.writeln("'ok'")
		}
		return
	}
	g.core_body_chain(stmts, 0, true)
}

// core_body_chain recursively generates a chain of let/do expressions.
// write_indent: whether to write leading indentation (false after 'in  ' prefix)
fn (mut g CoreGen) core_body_chain(stmts []ast.Stmt, idx int, write_indent bool) {
	if idx >= stmts.len {
		if g.etf_mode {
			g.emit_atom('ok')
		} else {
			if write_indent {
				g.write_indent_core()
			}
			g.out.writeln("'ok'")
		}
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
			// Array append: arr << val -> let <Arr_N> = erlang:'++'(OldArr, [Val|[]]) in REST
			if stmt.expr is ast.InfixExpr {
				infix := stmt.expr
				if infix.op == .left_shift {
					left_type_sym := g.table.sym(infix.left_type)
					if left_type_sym.kind == .array || left_type_sym.name.starts_with('[]') {
						if infix.left is ast.Ident {
							arr_name := (infix.left as ast.Ident).name
							// Capture old SSA variable BEFORE next_core_var updates var_map
							old_var := g.core_var(arr_name)
							new_var := g.next_core_var(arr_name)
							g.begin_let(new_var)
							g.begin_call('erlang', '++')
							g.emit_var(old_var)
							g.emit_sep()
							g.begin_cons()
							g.core_expr(infix.right)
							g.mid_cons()
							g.emit_nil()
							g.end_cons()
							g.end_call()
							g.mid_let()
							g.core_body_chain(stmts, idx + 1, false)
							g.end_let()
							return
						}
					}
				}
			}
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
			if g.etf_mode {
				g.begin_seq()
				g.core_expr(stmt.expr)
				g.mid_seq()
				g.core_body_chain(stmts, idx + 1, false)
				g.end_seq()
			} else {
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
		}
		ast.Return {
			// Early return - generate expression (remaining stmts are dead code)
			g.core_tail_stmt(stmt, write_indent)
		}
		ast.ForInStmt {
			if g.etf_mode {
				// ETF mode: for-in loops use same logic but with ETF emission
				// For now, delegate to text mode patterns (they call core_expr which handles ETF)
				base_accs := g.core_find_loop_accumulators(stmt.stmts)
				has_index := !stmt.is_range && stmt.key_var.len > 0
					&& stmt.key_var != '_'
				if base_accs.len > 0 {
					mut init_vals := []string{}
					for acc in base_accs {
						init_vals << g.core_var(acc)
					}
					index_var := if has_index { stmt.key_var } else { '' }
					mut all_accs := base_accs.clone()
					if has_index {
						_ = g.core_var(stmt.key_var)
						all_accs << stmt.key_var
						init_vals << '0'
					}
					if has_index {
						tuple_temp := g.new_temp()
						new_var := g.next_core_var(base_accs[0])
						g.begin_let(tuple_temp)
						g.core_for_foldl(stmt, all_accs, init_vals, index_var)
						g.mid_let()
						g.begin_let(new_var)
						g.begin_call('erlang', 'element')
						g.emit_int('1')
						g.write_core(', ')
						g.emit_var(tuple_temp)
						g.end_call()
						g.mid_let()
						g.core_body_chain(stmts, idx + 1, false)
						g.end_let()
						g.end_let()
					} else {
						new_var := g.next_core_var(all_accs[0])
						g.begin_let(new_var)
						g.core_for_foldl(stmt, all_accs, init_vals, '')
						g.mid_let()
						g.core_body_chain(stmts, idx + 1, false)
						g.end_let()
					}
				} else {
					g.begin_seq()
					g.core_for_in(stmt)
					g.mid_seq()
					g.core_body_chain(stmts, idx + 1, false)
					g.end_seq()
				}
			} else {
				base_accs := g.core_find_loop_accumulators(stmt.stmts)
				has_index := !stmt.is_range && stmt.key_var.len > 0
					&& stmt.key_var != '_'
				if base_accs.len > 0 {
					// Loop with accumulators: bind result to accumulator var
					if write_indent {
						g.write_indent_core()
					}
					mut init_vals := []string{}
					for acc in base_accs {
						init_vals << g.core_var(acc)
					}
					index_var := if has_index { stmt.key_var } else { '' }
					mut all_accs := base_accs.clone()
					if has_index {
						_ = g.core_var(stmt.key_var)
						all_accs << stmt.key_var
						init_vals << '0'
					}
					if has_index {
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
		}
		ast.ForStmt {
			// While loop: for cond { body }
			// -> letrec 'loop_N'/K = fun(Acc1,...,AccK) ->
			//        case COND of 'true' -> do BODY apply 'loop_N'/K(NewAcc...)
			//                     'false' -> {Acc1,...,AccK} end
			//    in let {Acc1',...} = apply 'loop_N'/K(Init...) in REST
			g.core_while_loop(stmt, stmts, idx + 1)
		}
		ast.ForCStmt {
			// C-style for: for init; cond; inc { body }
			// Similar to while but with init and increment
			g.core_c_style_loop(stmt, stmts, idx + 1)
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

	if g.etf_mode {
		// ETF mode: use let/seq helpers (no indentation management)
		for i, left in node.left {
			if left is ast.Ident {
				if left.name == '_' {
					// Discard: seq(EXPR, REST)
					rhs_idx := if i < node.right.len { i } else { 0 }
					g.begin_seq()
					g.core_expr(node.right[rhs_idx])
					g.mid_seq()
					if i == node.left.len - 1 {
						g.core_body_chain(stmts, next_idx, false)
					} else {
						g.emit_atom('ok')
					}
					g.end_seq()
					continue
				}

				rhs_idx := if i < node.right.len { i } else { 0 }

				// Identity binding optimization
				rhs_expr := node.right[rhs_idx]
				if rhs_expr is ast.Ident {
					if rhs_expr.name in g.var_map {
						g.var_map[left.name] = g.var_map[rhs_expr.name]
						if i == node.left.len - 1 {
							g.core_body_chain(stmts, next_idx, false)
						}
						continue
					}
				}

				old_var_name := if left.name in g.var_map {
					g.var_map[left.name]
				} else {
					''
				}
				var_name := g.next_core_var(left.name)

				g.begin_let(var_name)
				if old_var_name.len > 0 {
					g.var_map[left.name] = old_var_name
				}
				g.core_expr(node.right[rhs_idx])
				g.var_map[left.name] = var_name
				g.mid_let()

				if i == node.left.len - 1 {
					g.core_body_chain(stmts, next_idx, false)
				}
				g.end_let()
			} else if left is ast.IndexExpr {
				// Map mutation: m[k] = v -> let <NewMap> = maps:put(K, V, OldMap) in REST
				index_expr := left as ast.IndexExpr
				if index_expr.left is ast.Ident {
					map_name := (index_expr.left as ast.Ident).name
					map_type_sym := g.table.sym(index_expr.left_type)
					if map_type_sym.kind == .map || map_type_sym.name.starts_with('map[') {
						rhs_idx := if i < node.right.len { i } else { 0 }
						old_var := g.core_var(map_name)
						new_var := g.next_core_var(map_name)
						g.begin_let(new_var)
						g.begin_call('maps', 'put')
						g.core_expr(index_expr.index)
						g.emit_sep()
						g.core_expr(node.right[rhs_idx])
						g.emit_sep()
						g.emit_var(old_var)
						g.end_call()
						g.mid_let()
						if i == node.left.len - 1 {
							g.core_body_chain(stmts, next_idx, false)
						}
						g.end_let()
					} else {
						// Non-map index assignment - side effect
						rhs_idx := if i < node.right.len { i } else { 0 }
						g.begin_seq()
						g.core_expr(node.right[rhs_idx])
						g.mid_seq()
						if i == node.left.len - 1 {
							g.core_body_chain(stmts, next_idx, false)
						} else {
							g.emit_atom('ok')
						}
						g.end_seq()
					}
				} else {
					rhs_idx := if i < node.right.len { i } else { 0 }
					g.begin_seq()
					g.core_expr(node.right[rhs_idx])
					g.mid_seq()
					if i == node.left.len - 1 {
						g.core_body_chain(stmts, next_idx, false)
					} else {
						g.emit_atom('ok')
					}
					g.end_seq()
				}
			} else {
				// Non-ident left side - treat as side effect
				rhs_idx := if i < node.right.len { i } else { 0 }
				g.begin_seq()
				g.core_expr(node.right[rhs_idx])
				g.mid_seq()
				if i == node.left.len - 1 {
					g.core_body_chain(stmts, next_idx, false)
				} else {
					g.emit_atom('ok')
				}
				g.end_seq()
			}
		}
	} else {
		// Text mode: manual indentation management
		mut let_count := 0
		mut needs_indent := write_indent
		for i, left in node.left {
			if left is ast.Ident {
				if left.name == '_' {
					// Discard: do EXPR REST
					if needs_indent {
						g.write_indent_core()
						needs_indent = false
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

				// Identity binding optimization
				rhs_expr := node.right[rhs_idx]
				if rhs_expr is ast.Ident {
					if rhs_expr.name in g.var_map {
						g.var_map[left.name] = g.var_map[rhs_expr.name]
						if i == node.left.len - 1 {
							g.core_body_chain(stmts, next_idx, needs_indent)
						}
						continue
					}
				}

				old_var_name := if left.name in g.var_map {
					g.var_map[left.name]
				} else {
					''
				}
				var_name := g.next_core_var(left.name)

				if needs_indent {
					g.write_indent_core()
					needs_indent = false
				}
				g.out.write_string('let <${var_name}> =')
				g.out.writeln('')
				g.indent++
				g.write_indent_core()
				if old_var_name.len > 0 {
					g.var_map[left.name] = old_var_name
				}
				g.core_expr(node.right[rhs_idx])
				g.var_map[left.name] = var_name
				g.out.writeln('')
				g.indent--
				g.write_indent_core()
				g.out.write_string('in  ')
				g.indent++
				let_count++

				if i == node.left.len - 1 {
					g.core_body_chain(stmts, next_idx, false)
				}
			} else if left is ast.IndexExpr {
				// Map mutation: m[k] = v -> let <NewMap> = maps:put(K, V, OldMap) in REST
				index_expr := left as ast.IndexExpr
				if index_expr.left is ast.Ident {
					map_name := (index_expr.left as ast.Ident).name
					map_type_sym := g.table.sym(index_expr.left_type)
					if map_type_sym.kind == .map || map_type_sym.name.starts_with('map[') {
						rhs_idx := if i < node.right.len { i } else { 0 }
						old_var := g.core_var(map_name)
						new_var := g.next_core_var(map_name)
						if needs_indent {
							g.write_indent_core()
							needs_indent = false
						}
						g.out.write_string('let <${new_var}> =')
						g.out.writeln('')
						g.indent++
						g.write_indent_core()
						g.out.write_string("call 'maps':'put'(")
						g.core_expr(index_expr.index)
						g.out.write_string(', ')
						g.core_expr(node.right[rhs_idx])
						g.out.write_string(', ${old_var})')
						g.out.writeln('')
						g.indent--
						g.write_indent_core()
						g.out.write_string('in  ')
						g.indent++
						let_count++
						if i == node.left.len - 1 {
							g.core_body_chain(stmts, next_idx, false)
						}
					} else {
						// Non-map index — side effect
						if needs_indent {
							g.write_indent_core()
							needs_indent = false
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
				} else {
					if needs_indent {
						g.write_indent_core()
						needs_indent = false
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
			} else {
				// Non-ident left side - treat as side effect
				if needs_indent {
					g.write_indent_core()
					needs_indent = false
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
}

// core_tail_stmt generates a statement in tail (return) position.
fn (mut g CoreGen) core_tail_stmt(stmt ast.Stmt, write_indent bool) {
	if g.etf_mode {
		// ETF mode: no indentation, use helpers
		match stmt {
			ast.Return {
				if stmt.exprs.len > 0 {
					g.core_expr(stmt.exprs[0])
				} else {
					g.emit_atom('ok')
				}
			}
			ast.ExprStmt {
				g.core_expr(stmt.expr)
			}
			ast.AssignStmt {
				if stmt.left.len > 0 && stmt.left[0] is ast.Ident {
					ident := stmt.left[0] as ast.Ident
					old_var_name := if ident.name in g.var_map {
						g.var_map[ident.name]
					} else {
						''
					}
					var_name := g.next_core_var(ident.name)
					g.begin_let(var_name)
					if old_var_name.len > 0 {
						g.var_map[ident.name] = old_var_name
					}
					g.core_expr(stmt.right[0])
					g.var_map[ident.name] = var_name
					g.mid_let()
					g.emit_var(var_name)
					g.end_let()
				} else {
					g.emit_atom('ok')
				}
			}
			else {
				g.emit_atom('ok')
			}
		}
	} else {
		// Text mode: with indentation
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
				if stmt.left.len > 0 && stmt.left[0] is ast.Ident {
					ident := stmt.left[0] as ast.Ident
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

	if g.etf_mode {
		// ETF mode: let <Fun> = fun(LoopVar, AccIn) -> ... in let <Seq> = ... in foldl(...)
		g.begin_let(fun_temp)
		if accumulators.len == 1 {
			g.begin_fun([loop_var, acc_in[0]])
		} else {
			// Use a single tuple param and destructure with element/2
			acc_tuple := g.new_temp()
			g.begin_fun([loop_var, acc_tuple])
			// Destructure: let <Ain_j> = call erlang:element(j+1, AccTuple) in ...
			for j, ain in acc_in {
				g.begin_let(ain)
				g.begin_call('erlang', 'element')
				g.emit_int('${j + 1}')
				g.emit_sep()
				g.emit_var(acc_tuple)
				g.end_call()
				g.mid_let()
			}
		}

		// Generate fold body: compute new accumulator values
		g.core_fold_body(node.stmts, accumulators, acc_in, acc_out, index_var)

		// Close destructuring lets for multi-acc
		if accumulators.len > 1 {
			for _ in acc_in {
				g.end_let()
			}
		}
		g.end_fun()
		g.mid_let()

		// let <Seq> = call 'lists':'seq'(Low, High - 1) in ...
		if node.is_range {
			g.begin_let(seq_temp)
			g.begin_call('lists', 'seq')
			g.core_expr(node.cond)
			g.emit_sep()
			g.begin_call('erlang', '-')
			g.core_expr(node.high)
			g.emit_sep()
			g.emit_int('1')
			g.end_call()
			g.end_call()
			g.mid_let()
		}

		// call 'lists':'foldl'(Fun, InitVal, Seq)
		g.begin_call('lists', 'foldl')
		g.emit_var(fun_temp)
		g.emit_sep()
		if accumulators.len == 1 {
			g.emit_var(init_vals[0])
		} else {
			g.begin_tuple()
			for j, iv in init_vals {
				if j > 0 {
					g.emit_sep()
				}
				g.emit_var(iv)
			}
			g.end_tuple()
		}
		g.emit_sep()
		if node.is_range {
			g.emit_var(seq_temp)
		} else {
			g.core_expr(node.cond)
		}
		g.end_call()

		// Close seq let if range
		if node.is_range {
			g.end_let()
		}
		g.end_let()
	} else {
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

	if g.etf_mode {
		// ETF mode: generate let bindings and seq for accumulator body
		// Track open wrappers (let/seq) so we can close them
		mut open_lets := 0
		mut open_seqs := 0

		for s_idx, stmt in stmts {
			_ = s_idx
			match stmt {
				ast.AssignStmt {
					for j, left in stmt.left {
						if left is ast.Ident {
							acc_idx := accumulators.index(left.name)
							if acc_idx >= 0 {
								rhs_idx := if j < stmt.right.len { j } else { 0 }
								new_name := acc_out[acc_idx]
								g.begin_let(new_name)
								g.core_expr(stmt.right[rhs_idx])
								g.mid_let()
								g.var_map[left.name] = new_name
								open_lets++
							} else {
								rhs_idx := if j < stmt.right.len { j } else { 0 }
								var_name := g.next_core_var(left.name)
								g.begin_let(var_name)
								g.core_expr(stmt.right[rhs_idx])
								g.mid_let()
								open_lets++
							}
						}
					}
				}
				ast.ExprStmt {
					// Check if this is an if-block that modifies accumulators
					if stmt.expr is ast.IfExpr {
						modified := g.core_find_if_modified_vars(stmt.expr)
						mut acc_modified := []string{}
						for m in modified {
							if accumulators.index(m) >= 0 {
								acc_modified << m
							}
						}
						if acc_modified.len > 0 {
							if acc_modified.len == 1 {
								mvar := acc_modified[0]
								acc_idx := accumulators.index(mvar)
								if acc_idx >= 0 {
									old_val := g.var_map[mvar]
									new_name := acc_out[acc_idx]
									g.begin_let(new_name)
									g.core_phi_if_branches(stmt.expr.branches, 0, mvar, old_val)
									g.mid_let()
									g.var_map[mvar] = new_name
									open_lets++
								}
							} else {
								tuple_temp := g.new_temp()
								g.begin_let(tuple_temp)
								g.core_phi_multi_if(stmt.expr, acc_modified, accumulators)
								g.mid_let()
								open_lets++
								// Extract each element from tuple
								for j, mvar in acc_modified {
									acc_idx := accumulators.index(mvar)
									if acc_idx < 0 {
										continue
									}
									new_name := acc_out[acc_idx]
									g.begin_let(new_name)
									g.begin_call('erlang', 'element')
									g.emit_int('${j + 1}')
									g.emit_sep()
									g.emit_var(tuple_temp)
									g.end_call()
									g.mid_let()
									g.var_map[mvar] = new_name
									open_lets++
								}
							}
							continue
						}
					}
					// Regular side effect
					g.begin_seq()
					g.core_expr(stmt.expr)
					g.mid_seq()
					open_seqs++
				}
				else {
					g.begin_seq()
					g.emit_atom('ok')
					g.mid_seq()
					open_seqs++
				}
			}
		}

		// Return the accumulator(s)
		if accumulators.len == 1 {
			if index_var.len > 0 && accumulators[0] == index_var {
				g.begin_call('erlang', '+')
				g.emit_var(g.var_map[accumulators[0]])
				g.emit_sep()
				g.emit_int('1')
				g.end_call()
			} else {
				g.emit_var(g.var_map[accumulators[0]])
			}
		} else {
			g.begin_tuple()
			for j, acc in accumulators {
				if j > 0 {
					g.emit_sep()
				}
				if index_var.len > 0 && acc == index_var {
					g.begin_call('erlang', '+')
					g.emit_var(g.var_map[acc])
					g.emit_sep()
					g.emit_int('1')
					g.end_call()
				} else {
					g.emit_var(g.var_map[acc])
				}
			}
			g.end_tuple()
		}

		// Close open wrappers
		for _ in 0 .. open_seqs {
			g.end_seq()
		}
		for _ in 0 .. open_lets {
			g.end_let()
		}
	} else {
		// Text mode: manual indentation management
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

	if g.etf_mode {
		// ETF mode: let <Fun> = fun(LoopVar) -> body in let <Seq> = seq(...) in foreach(Fun, Seq)
		g.begin_let(fun_temp)
		g.begin_fun([loop_var])
		if node.stmts.len == 0 {
			g.emit_atom('ok')
		} else {
			g.core_fn_body(node.stmts)
		}
		g.end_fun()
		g.mid_let()

		// let <Seq> = call 'lists':'seq'(Low, High-1)
		g.begin_let(seq_temp)
		g.begin_call('lists', 'seq')
		g.core_expr(node.cond)
		g.emit_sep()
		g.begin_call('erlang', '-')
		g.core_expr(node.high)
		g.emit_sep()
		g.emit_int('1')
		g.end_call()
		g.end_call()
		g.mid_let()

		// call 'lists':'foreach'(Fun, Seq)
		g.begin_call('lists', 'foreach')
		g.emit_var(fun_temp)
		g.emit_sep()
		g.emit_var(seq_temp)
		g.end_call()

		g.end_let() // seq let
		g.end_let() // fun let
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
}

fn (mut g CoreGen) core_for_collection_foreach(node ast.ForInStmt) {
	has_index := node.key_var.len > 0 && node.key_var != '_'
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()

	if g.etf_mode {
		if has_index {
			// Indexed loop: for i, val in collection
			// let <Fun> = fun(Val, Idx) -> do BODY erlang:'+'(Idx, 1)
			// in lists:foldl(Fun, 0, List)
			idx_var := g.core_var(node.key_var)
			pair_temp := g.new_temp()
			_ = pair_temp
			g.begin_let(fun_temp)
			g.begin_fun([loop_var, idx_var])
			if node.stmts.len == 0 {
				g.begin_call('erlang', '+')
				g.emit_var(idx_var)
				g.emit_sep()
				g.emit_int('1')
				g.end_call()
			} else {
				// Generate body as side effects, then return idx + 1
				for stmt in node.stmts {
					g.begin_seq()
					match stmt {
						ast.ExprStmt { g.core_expr(stmt.expr) }
						ast.AssignStmt {
							if stmt.right.len > 0 {
								g.core_expr(stmt.right[0])
							} else {
								g.emit_atom('ok')
							}
						}
						else { g.emit_atom('ok') }
					}
					g.mid_seq()
				}
				g.begin_call('erlang', '+')
				g.emit_var(idx_var)
				g.emit_sep()
				g.emit_int('1')
				g.end_call()
				for _ in node.stmts {
					g.end_seq()
				}
			}
			g.end_fun()
			g.mid_let()

			// lists:foldl(Fun, 0, List)
			g.begin_call('lists', 'foldl')
			g.emit_var(fun_temp)
			g.emit_sep()
			g.emit_int('0')
			g.emit_sep()
			g.core_expr(node.cond)
			g.end_call()

			g.end_let()
		} else {
			// let <Fun> = fun(LoopVar) -> body in lists:foreach(Fun, Coll)
			g.begin_let(fun_temp)
			g.begin_fun([loop_var])
			if node.stmts.len == 0 {
				g.emit_atom('ok')
			} else {
				g.core_fn_body(node.stmts)
			}
			g.end_fun()
			g.mid_let()

			g.begin_call('lists', 'foreach')
			g.emit_var(fun_temp)
			g.emit_sep()
			g.core_expr(node.cond)
			g.end_call()

			g.end_let()
		}
	} else {
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

	if g.etf_mode {
		// Handle IfGuardExpr: if val := expr { return val } rest...
		if branch.cond is ast.IfGuardExpr {
			g.core_early_return_guard(branch.cond, branch, stmts, next_idx)
			return
		}

		// case COND of <'true'> -> RETURN_VAL ; <'false'> -> REST end
		g.begin_case()
		g.core_expr(branch.cond)
		g.mid_case_clauses()
		// true clause
		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_branch_return_val(branch)
		g.end_clause()
		g.clause_sep()
		// false clause
		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		remaining := stmts[next_idx..]
		if remaining.len == 0 {
			g.emit_atom('ok')
		} else {
			g.core_body_chain(remaining, 0, false)
		}
		g.end_clause()
		g.end_case()
	} else {
		if write_indent {
			g.write_indent_core()
		}

		// Handle IfGuardExpr: if val := expr { return val } rest...
		if branch.cond is ast.IfGuardExpr {
			g.core_early_return_guard(branch.cond, branch, stmts, next_idx)
			return
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
}

fn (mut g CoreGen) core_early_return_guard(guard ast.IfGuardExpr, branch ast.IfBranch, stmts []ast.Stmt, next_idx int) {
	// if val := expr { return val } rest...
	// Becomes: case maps:find(K,M) of <{ok,Val}> -> Val <_> -> REST end

	// Register guard variables
	mut var_names := []string{}
	for gvar in guard.vars {
		vname := g.next_core_var(gvar.name)
		var_names << vname
	}

	// Determine if this is a map lookup
	is_map_lookup := if guard.expr is ast.IndexExpr {
		guard.expr.left_type.idx() != 0
			&& g.table.sym(guard.expr.left_type).kind == .map
	} else {
		false
	}

	if g.etf_mode {
		// ETF mode: case EXPR of <{ok, Val}> -> ... ; <_> -> REST end
		g.begin_case()
		if is_map_lookup {
			index_expr := guard.expr as ast.IndexExpr
			g.begin_call('maps', 'find')
			g.core_expr(index_expr.index)
			g.emit_sep()
			g.core_expr(index_expr.left)
			g.end_call()
		} else {
			// General optional: wrap in try/catch
			g.temp_counter++
			tmp := '_cor${g.temp_counter}'
			g.begin_try()
			g.core_expr(guard.expr)
			g.mid_try_of(tmp)
			// success: {ok, Tmp}
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_var(tmp)
			g.end_tuple()
			g.mid_try_catch('_cor_c', '_cor_r', '_cor_s')
			g.emit_atom('error')
			g.end_try()
		}
		g.mid_case_clauses()

		// Success clause: <{ok, Val}> when true -> RETURN_VAL
		g.begin_clause()
		if var_names.len == 1 {
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.emit_var(var_names[0])
			g.end_tuple()
		} else {
			g.begin_tuple()
			g.emit_atom('ok')
			g.emit_sep()
			g.temp_counter++
			g.emit_var('_cor${g.temp_counter}')
			g.end_tuple()
		}
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		g.core_branch_return_val(branch)
		g.end_clause()
		g.clause_sep()

		// Failure clause: <_> when true -> REST
		g.begin_clause()
		g.temp_counter++
		g.emit_var('_cor${g.temp_counter}')
		g.mid_clause_guard()
		g.emit_true_guard()
		g.mid_clause_body()
		remaining := stmts[next_idx..]
		if remaining.len == 0 {
			g.emit_atom('ok')
		} else {
			g.core_body_chain(remaining, 0, false)
		}
		g.end_clause()
		g.end_case()
	} else {
		if is_map_lookup {
			index_expr := guard.expr as ast.IndexExpr
			g.out.write_string("case call 'maps':'find'(")
			g.core_expr(index_expr.index)
			g.out.write_string(', ')
			g.core_expr(index_expr.left)
			g.out.writeln(') of')
		} else {
			// General optional: wrap in try/catch
			g.temp_counter++
			tmp := '_cor${g.temp_counter}'
			g.out.write_string('case try ')
			g.core_expr(guard.expr)
			g.out.write_string(' of <${tmp}> when \'true\' -> {\'ok\', ${tmp}}')
			g.out.writeln(' catch <_cor_c,_cor_r,_cor_s> when \'true\' -> \'error\' of')
		}

		g.indent++

		// Success branch: {ok, Val} -> return val
		g.write_indent_core()
		if var_names.len == 1 {
			g.out.write_string("<{'ok', ${var_names[0]}}> when 'true' -> ")
		} else {
			g.out.write_string("<{'ok', _}> when 'true' -> ")
		}
		g.core_branch_return_val(branch)
		g.out.writeln('')

		// Failure branch: _ -> REST
		g.write_indent_core()
		g.out.write_string("<_> when 'true' -> ")
		remaining := stmts[next_idx..]
		if remaining.len == 0 {
			g.out.writeln("'ok'")
		} else {
			g.out.writeln('')
			g.indent++
			g.core_body_chain(remaining, 0, true)
			g.indent--
		}

		g.indent--
		g.write_indent_core()
		g.out.writeln('end')
	}
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
	// Fallback: return 'ok' atom
	g.emit_atom('ok')
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
	if g.etf_mode {
		// ETF mode: nested let bindings around case expressions for modified vars
		for _, mvar in modified {
			old_val := g.core_var(mvar)
			new_var := g.next_core_var(mvar)

			// Temporarily restore old var mapping so the case condition
			// and branch body reference the OLD variable, not the new one.
			g.var_map[mvar] = old_val

			g.begin_let(new_var)

			// Generate case expression that returns the new or old value
			g.core_phi_if_branches(node.branches, 0, mvar, old_val)

			// Restore new var mapping for subsequent code
			g.var_map[mvar] = new_var

			g.mid_let()
		}

		// Continue with remaining statements
		g.core_body_chain(stmts, next_idx, false)

		for _ in modified {
			g.end_let()
		}
	} else {
		// Text mode: nested let bindings with indentation
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
}

// core_phi_if_branches generates case branches that return the modified variable value.
fn (mut g CoreGen) core_phi_if_branches(branches []ast.IfBranch, idx int, target_var string, old_val string) {
	if idx >= branches.len {
		if g.etf_mode {
			g.emit_var(old_val)
		} else {
			g.write_core(old_val)
		}
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if g.etf_mode {
		if is_else {
			g.core_phi_branch_body(branch, target_var, old_val)
		} else {
			g.begin_case()
			g.core_expr(branch.cond)
			g.mid_case_clauses()
			// true clause
			g.begin_clause()
			g.emit_atom('true')
			g.mid_clause_guard()
			g.emit_true_guard()
			g.mid_clause_body()
			g.core_phi_branch_body(branch, target_var, old_val)
			g.end_clause()
			g.clause_sep()
			// false clause
			g.begin_clause()
			g.emit_atom('false')
			g.mid_clause_guard()
			g.emit_true_guard()
			g.mid_clause_body()
			if idx + 1 < branches.len {
				g.core_phi_if_branches(branches, idx + 1, target_var, old_val)
			} else {
				g.emit_var(old_val)
			}
			g.end_clause()
			g.end_case()
		}
	} else {
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
}

// core_phi_branch_body generates a branch body that executes all statements
// and returns the target variable's value (or old value if not modified).
fn (mut g CoreGen) core_phi_branch_body(branch ast.IfBranch, target_var string, old_val string) {
	if branch.stmts.len == 0 {
		if g.etf_mode {
			g.emit_var(old_val)
		} else {
			g.write_core(old_val)
		}
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

	if g.etf_mode {
		if last_assign_idx < 0 {
			// Target not modified in this branch - generate side effects and return old
			if branch.stmts.len == 1 {
				first := branch.stmts[0]
				if first is ast.ExprStmt {
					g.begin_seq()
					g.core_expr(first.expr)
					g.mid_seq()
					g.emit_var(old_val)
					g.end_seq()
				} else {
					g.emit_var(old_val)
				}
			} else {
				g.emit_var(old_val)
			}
			return
		}

		// Generate all statements up to and including the assignment.
		// Side-effect statements become seq, assignments become let.
		for i, stmt in branch.stmts {
			if i > last_assign_idx {
				break
			}
			if stmt is ast.AssignStmt {
				for j, left in stmt.left {
					if left is ast.Ident && left.name == target_var {
						rhs_idx := if j < stmt.right.len { j } else { 0 }
						if i < last_assign_idx {
							// Not the final assignment - use seq
							g.begin_seq()
							g.core_expr(stmt.right[rhs_idx])
							g.mid_seq()
						} else {
							// Final assignment to target - this becomes the return value
							g.core_expr(stmt.right[rhs_idx])
						}
					} else if left is ast.Ident {
						// Other variable assignment - generate let binding
						var_name := g.next_core_var(left.name)
						rhs_idx := if j < stmt.right.len { j } else { 0 }
						g.begin_let(var_name)
						g.core_expr(stmt.right[rhs_idx])
						g.mid_let()
					}
				}
			} else if stmt is ast.ExprStmt {
				g.begin_seq()
				g.core_expr(stmt.expr)
				g.mid_seq()
			}
		}

		// Close any open seq/let wrappers for non-final assignments and side effects
		for i, stmt in branch.stmts {
			if i > last_assign_idx {
				break
			}
			if stmt is ast.AssignStmt {
				for _, left in stmt.left {
					if left is ast.Ident && left.name == target_var {
						if i < last_assign_idx {
							g.end_seq()
						}
					} else if left is ast.Ident {
						g.end_let()
					}
				}
			} else if stmt is ast.ExprStmt {
				g.end_seq()
			}
		}
	} else {
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
		if g.etf_mode {
			g.begin_tuple()
			for j, tv in target_vars {
				if j > 0 {
					g.emit_sep()
				}
				g.emit_var(g.core_var(tv))
			}
			g.end_tuple()
		} else {
			g.write_core('{')
			for j, tv in target_vars {
				if j > 0 {
					g.write_core(', ')
				}
				g.write_core(g.core_var(tv))
			}
			g.write_core('}')
		}
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if g.etf_mode {
		if is_else {
			g.core_phi_multi_body(branch, target_vars)
		} else {
			g.begin_case()
			g.core_expr(branch.cond)
			g.mid_case_clauses()
			// true clause
			g.begin_clause()
			g.emit_atom('true')
			g.mid_clause_guard()
			g.emit_true_guard()
			g.mid_clause_body()
			g.core_phi_multi_body(branch, target_vars)
			g.end_clause()
			g.clause_sep()
			// false clause
			g.begin_clause()
			g.emit_atom('false')
			g.mid_clause_guard()
			g.emit_true_guard()
			g.mid_clause_body()
			if idx + 1 < branches.len {
				g.core_phi_multi_branches(branches, idx + 1, target_vars)
			} else {
				// No else - return old values as tuple
				g.begin_tuple()
				for j, tv in target_vars {
					if j > 0 {
						g.emit_sep()
					}
					g.emit_var(g.core_var(tv))
				}
				g.end_tuple()
			}
			g.end_clause()
			g.end_case()
		}
	} else {
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

	if g.etf_mode {
		// Generate side-effect statements (ExprStmt) before the last assignment
		// Track how many seq wrappers we open
		mut seq_count := 0
		for i, stmt in branch.stmts {
			if i > max_assign {
				break
			}
			if stmt is ast.ExprStmt {
				g.begin_seq()
				g.core_expr(stmt.expr)
				g.mid_seq()
				seq_count++
			}
		}

		// Generate tuple of values
		g.begin_tuple()
		for j, tv in target_vars {
			if j > 0 {
				g.emit_sep()
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
						g.emit_var(g.core_var(tv))
					}
				} else {
					g.emit_var(g.core_var(tv))
				}
			} else {
				// Not modified in this branch - return old value
				g.emit_var(g.core_var(tv))
			}
		}
		g.end_tuple()

		// Close seq wrappers
		for _ in 0 .. seq_count {
			g.end_seq()
		}
	} else {
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
}

// core_while_loop generates a while loop using letrec.
fn (mut g CoreGen) core_while_loop(node ast.ForStmt, stmts []ast.Stmt, next_idx int) {
	base_accs := g.core_find_loop_accumulators(node.stmts)
	g.temp_counter++
	loop_name := 'loop_${g.temp_counter}'
	arity := base_accs.len

	mut param_names := []string{}
	mut init_vals := []string{}
	for acc in base_accs {
		param_names << '${acc.capitalize()}Param'
		init_vals << g.core_var(acc)
	}

	if base_accs.len == 0 {
		// No accumulators — simple side-effect loop
		g.begin_letrec(loop_name, 0)
		g.mid_letrec_body()
		g.begin_case()
		g.core_expr(node.cond)
		g.mid_case_clauses()
		// true -> do BODY apply loop/0()
		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_atom('true')
		g.mid_clause_body()
		for body_stmt in node.stmts {
			g.begin_seq()
			g.core_tail_stmt(body_stmt, false)
			g.mid_seq()
		}
		g.begin_apply(loop_name, 0)
		g.end_apply()
		for _ in node.stmts {
			g.end_seq()
		}
		g.end_clause()
		g.clause_sep()
		// false -> 'ok'
		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_atom('true')
		g.mid_clause_body()
		g.emit_atom('ok')
		g.end_clause()
		g.end_case()
		g.mid_letrec_after()
		g.begin_seq()
		g.begin_apply(loop_name, 0)
		g.end_apply()
		g.mid_seq()
		g.core_body_chain(stmts, next_idx, false)
		g.end_seq()
		g.end_letrec()
	} else {
		mut saved := map[string]string{}
		for acc in base_accs {
			if acc in g.var_map {
				saved[acc] = g.var_map[acc]
			}
		}

		mut out_names := []string{}
		for acc in base_accs {
			g.temp_counter++
			out_names << '${acc.capitalize()}Out_${g.temp_counter}'
		}

		g.begin_letrec(loop_name, arity)
		for i, pn in param_names {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_letrec_param(pn)
		}
		g.mid_letrec_body()

		for i, acc in base_accs {
			g.var_map[acc] = param_names[i]
		}

		g.begin_case()
		g.core_expr(node.cond)
		g.mid_case_clauses()

		g.begin_clause()
		g.emit_atom('true')
		g.mid_clause_guard()
		g.emit_atom('true')
		g.mid_clause_body()
		g.core_loop_body_and_recurse(node.stmts, base_accs, param_names, out_names, loop_name, arity)
		g.end_clause()
		g.clause_sep()

		g.begin_clause()
		g.emit_atom('false')
		g.mid_clause_guard()
		g.emit_atom('true')
		g.mid_clause_body()
		if arity == 1 {
			g.emit_var(param_names[0])
		} else {
			g.begin_tuple()
			for i, pn in param_names {
				if i > 0 {
					g.emit_sep()
				}
				g.emit_var(pn)
			}
			g.end_tuple()
		}
		g.end_clause()
		g.end_case()
		g.mid_letrec_after()

		for acc in base_accs {
			if acc in saved {
				g.var_map[acc] = saved[acc]
			}
		}

		if arity == 1 {
			new_var := g.next_core_var(base_accs[0])
			g.begin_let(new_var)
			g.begin_apply(loop_name, arity)
			for i, iv in init_vals {
				if i > 0 {
					g.emit_sep()
				}
				g.emit_var(iv)
			}
			g.end_apply()
			g.mid_let()
			g.core_body_chain(stmts, next_idx, false)
			g.end_let()
		} else {
			result_temp := g.new_temp()
			g.begin_let(result_temp)
			g.begin_apply(loop_name, arity)
			for i, iv in init_vals {
				if i > 0 {
					g.emit_sep()
				}
				g.emit_var(iv)
			}
			g.end_apply()
			g.mid_let()
			for i, acc in base_accs {
				new_var := g.next_core_var(acc)
				g.begin_let(new_var)
				g.begin_call('erlang', 'element')
				g.emit_int('${i + 1}')
				g.emit_sep()
				g.emit_var(result_temp)
				g.end_call()
				g.mid_let()
			}
			g.core_body_chain(stmts, next_idx, false)
			for _ in base_accs {
				g.end_let()
			}
			g.end_let()
		}
		g.end_letrec()
	}
}

// core_c_style_loop generates a C-style for loop using letrec.
fn (mut g CoreGen) core_c_style_loop(node ast.ForCStmt, stmts []ast.Stmt, next_idx int) {
	base_accs := g.core_find_loop_accumulators(node.stmts)
	g.temp_counter++
	loop_name := 'loop_${g.temp_counter}'

	mut loop_var_name := ''
	if node.init is ast.AssignStmt {
		init_stmt := node.init as ast.AssignStmt
		if init_stmt.left.len > 0 && init_stmt.left[0] is ast.Ident {
			loop_var_name = (init_stmt.left[0] as ast.Ident).name
		}
	}

	mut accs := []string{}
	for acc in base_accs {
		if acc != loop_var_name {
			accs << acc
		}
	}

	arity := accs.len + 1
	loop_var_param := '${loop_var_name.capitalize()}Param'

	mut param_names := [loop_var_param]
	mut init_vals := []string{}
	for acc in accs {
		param_names << '${acc.capitalize()}Param'
		init_vals << g.core_var(acc)
	}

	// Emit init as let binding
	if node.init is ast.AssignStmt {
		init_stmt := node.init as ast.AssignStmt
		if init_stmt.left.len > 0 && init_stmt.left[0] is ast.Ident && init_stmt.right.len > 0 {
			init_var := g.next_core_var(loop_var_name)
			g.begin_let(init_var)
			g.core_expr(init_stmt.right[0])
			g.mid_let()
			init_vals.insert(0, init_var)
		}
	}

	mut saved := map[string]string{}
	for acc in accs {
		if acc in g.var_map {
			saved[acc] = g.var_map[acc]
		}
	}
	if loop_var_name in g.var_map {
		saved[loop_var_name] = g.var_map[loop_var_name]
	}

	mut out_names := []string{}
	for acc in accs {
		g.temp_counter++
		out_names << '${acc.capitalize()}Out_${g.temp_counter}'
	}

	g.begin_letrec(loop_name, arity)
	for i, pn in param_names {
		if i > 0 {
			g.emit_sep()
		}
		g.emit_letrec_param(pn)
	}
	g.mid_letrec_body()

	g.var_map[loop_var_name] = loop_var_param
	for i, acc in accs {
		g.var_map[acc] = param_names[i + 1]
	}

	g.begin_case()
	g.core_expr(node.cond)
	g.mid_case_clauses()

	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_atom('true')
	g.mid_clause_body()
	g.core_c_loop_body_and_recurse(node.stmts, accs, param_names, out_names,
		loop_name, arity, loop_var_name, loop_var_param, node.inc)
	g.end_clause()
	g.clause_sep()

	g.begin_clause()
	g.emit_atom('false')
	g.mid_clause_guard()
	g.emit_atom('true')
	g.mid_clause_body()
	if accs.len == 0 {
		g.emit_var(loop_var_param)
	} else {
		g.begin_tuple()
		for i, pn in param_names {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_var(pn)
		}
		g.end_tuple()
	}
	g.end_clause()
	g.end_case()
	g.mid_letrec_after()

	for acc in accs {
		if acc in saved {
			g.var_map[acc] = saved[acc]
		}
	}
	if loop_var_name in saved {
		g.var_map[loop_var_name] = saved[loop_var_name]
	}

	if accs.len == 0 {
		result_temp := g.new_temp()
		g.begin_let(result_temp)
		g.begin_apply(loop_name, arity)
		for i, iv in init_vals {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_var(iv)
		}
		g.end_apply()
		g.mid_let()
		g.core_body_chain(stmts, next_idx, false)
		g.end_let()
	} else {
		result_temp := g.new_temp()
		g.begin_let(result_temp)
		g.begin_apply(loop_name, arity)
		for i, iv in init_vals {
			if i > 0 {
				g.emit_sep()
			}
			g.emit_var(iv)
		}
		g.end_apply()
		g.mid_let()
		for i, acc in accs {
			new_var := g.next_core_var(acc)
			g.begin_let(new_var)
			g.begin_call('erlang', 'element')
			g.emit_int('${i + 2}')
			g.emit_sep()
			g.emit_var(result_temp)
			g.end_call()
			g.mid_let()
		}
		g.core_body_chain(stmts, next_idx, false)
		for _ in accs {
			g.end_let()
		}
		g.end_let()
	}
	// Close init let
	g.end_let()
	g.end_letrec()
}

// core_loop_body_and_recurse processes while loop body statements,
// updating accumulators and emitting a recursive apply at the end.
fn (mut g CoreGen) core_loop_body_and_recurse(body_stmts []ast.Stmt, accumulators []string, param_names []string, out_names []string, loop_name string, arity int) {
	mut open_wrappers := 0
	for stmt in body_stmts {
		match stmt {
			ast.AssignStmt {
				for j, left in stmt.left {
					if left is ast.Ident {
						acc_idx := accumulators.index(left.name)
						if acc_idx >= 0 {
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							new_name := out_names[acc_idx]
							g.begin_let(new_name)
							g.core_expr(stmt.right[rhs_idx])
							g.mid_let()
							g.var_map[left.name] = new_name
							open_wrappers++
						} else {
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							var_name := g.next_core_var(left.name)
							g.begin_let(var_name)
							g.core_expr(stmt.right[rhs_idx])
							g.mid_let()
							open_wrappers++
						}
					}
				}
			}
			ast.ExprStmt {
				g.begin_seq()
				g.core_expr(stmt.expr)
				g.mid_seq()
				open_wrappers++
			}
			else {
				g.begin_seq()
				g.core_tail_stmt(stmt, false)
				g.mid_seq()
				open_wrappers++
			}
		}
	}

	g.begin_apply(loop_name, arity)
	for i, acc in accumulators {
		if i > 0 {
			g.emit_sep()
		}
		g.emit_var(g.core_var(acc))
	}
	g.end_apply()

	for _ in 0 .. open_wrappers {
		if g.etf_mode {
			g.write_core('}')
		}
	}
}

// core_c_loop_body_and_recurse processes C-style for loop body,
// then emits increment and recursive apply.
fn (mut g CoreGen) core_c_loop_body_and_recurse(body_stmts []ast.Stmt, accumulators []string, param_names []string, out_names []string, loop_name string, arity int, loop_var_name string, loop_var_param string, inc_stmt ast.Stmt) {
	mut open_wrappers := 0
	for stmt in body_stmts {
		match stmt {
			ast.AssignStmt {
				for j, left in stmt.left {
					if left is ast.Ident {
						acc_idx := accumulators.index(left.name)
						if acc_idx >= 0 {
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							new_name := out_names[acc_idx]
							g.begin_let(new_name)
							g.core_expr(stmt.right[rhs_idx])
							g.mid_let()
							g.var_map[left.name] = new_name
							open_wrappers++
						} else {
							rhs_idx := if j < stmt.right.len { j } else { 0 }
							var_name := g.next_core_var(left.name)
							g.begin_let(var_name)
							g.core_expr(stmt.right[rhs_idx])
							g.mid_let()
							open_wrappers++
						}
					}
				}
			}
			ast.ExprStmt {
				g.begin_seq()
				g.core_expr(stmt.expr)
				g.mid_seq()
				open_wrappers++
			}
			else {
				g.begin_seq()
				g.core_tail_stmt(stmt, false)
				g.mid_seq()
				open_wrappers++
			}
		}
	}

	// Compute increment and recurse
	inc_temp := g.new_temp()
	g.begin_let(inc_temp)
	if inc_stmt is ast.AssignStmt {
		inc_assign := inc_stmt as ast.AssignStmt
		if inc_assign.right.len > 0 {
			g.core_expr(inc_assign.right[0])
		} else {
			g.begin_call('erlang', '+')
			g.emit_var(g.core_var(loop_var_name))
			g.emit_sep()
			g.emit_int('1')
			g.end_call()
		}
	} else {
		g.begin_call('erlang', '+')
		g.emit_var(g.core_var(loop_var_name))
		g.emit_sep()
		g.emit_int('1')
		g.end_call()
	}
	g.mid_let()

	g.begin_apply(loop_name, arity)
	g.emit_var(inc_temp)
	for acc in accumulators {
		g.emit_sep()
		g.emit_var(g.core_var(acc))
	}
	g.end_apply()
	g.end_let()

	for _ in 0 .. open_wrappers {
		if g.etf_mode {
			g.write_core('}')
		}
	}
}
