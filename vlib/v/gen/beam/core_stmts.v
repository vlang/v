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
		g.core_tail_stmt(stmt, write_indent)
		return
	}

	// Non-tail: need let or do sequencing
	match stmt {
		ast.AssignStmt {
			g.core_let_chain(stmt, stmts, idx + 1, write_indent)
		}
		ast.ExprStmt {
			g.core_expr_stmt_chain(stmt, stmts, idx, write_indent)
		}
		ast.Return {
			g.core_tail_stmt(stmt, write_indent)
		}
		ast.ForInStmt {
			g.core_for_in_chain(stmt, stmts, idx, write_indent)
		}
		ast.ForStmt {
			g.core_while_loop(stmt, stmts, idx + 1)
		}
		ast.ForCStmt {
			g.core_c_style_loop(stmt, stmts, idx + 1)
		}
		else {
			g.core_body_chain(stmts, idx + 1, write_indent)
		}
	}
}

// Handle ExprStmt in body chain: array append, early return, phi-if, or regular side effect
fn (mut g CoreGen) core_expr_stmt_chain(stmt ast.ExprStmt, stmts []ast.Stmt, idx int, write_indent bool) {
	// Array append: arr << val
	if stmt.expr is ast.InfixExpr {
		infix := stmt.expr
		if infix.op == .left_shift {
			if g.core_try_array_append(infix, stmts, idx + 1) {
				return
			}
		}
	}

	// Early-return-if or phi-if patterns
	if stmt.expr is ast.IfExpr {
		if_expr := stmt.expr
		if g.core_is_early_return_if(if_expr) {
			g.core_early_return_case(if_expr, stmts, idx + 1, write_indent)
			return
		}
		modified := g.core_find_if_modified_vars(if_expr)
		if modified.len > 0 {
			g.core_phi_if(if_expr, modified, stmts, idx + 1, write_indent)
			return
		}
	}

	// Regular side effect -> do EXPR REST
	g.core_regular_side_effect(stmt.expr, stmts, idx + 1, write_indent)
}

// Try to handle array append pattern, return true if handled
fn (mut g CoreGen) core_try_array_append(infix ast.InfixExpr, stmts []ast.Stmt, next_idx int) bool {
	left_type_sym := g.table.sym(infix.left_type)
	if left_type_sym.kind == .array || left_type_sym.name.starts_with('[]') {
		if infix.left is ast.Ident {
			arr_name := (infix.left as ast.Ident).name
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
			g.core_body_chain(stmts, next_idx, false)
			g.end_let()
			return true
		}
	}
	return false
}

// Handle regular side-effect expression in body chain
fn (mut g CoreGen) core_regular_side_effect(expr ast.Expr, stmts []ast.Stmt, next_idx int, write_indent bool) {
	if g.etf_mode {
		g.begin_seq()
		g.core_expr(expr)
		g.mid_seq()
		g.core_body_chain(stmts, next_idx, false)
		g.end_seq()
	} else {
		if write_indent {
			g.write_indent_core()
		}
		g.out.write_string('do  ')
		g.core_expr(expr)
		g.out.writeln('')
		g.indent++
		g.core_body_chain(stmts, next_idx, true)
		g.indent--
	}
}

// Handle ForInStmt in body chain
fn (mut g CoreGen) core_for_in_chain(stmt ast.ForInStmt, stmts []ast.Stmt, idx int, write_indent bool) {
	if g.etf_mode {
		g.core_for_in_chain_etf(stmt, stmts, idx + 1)
	} else {
		g.core_for_in_chain_text(stmt, stmts, idx + 1, write_indent)
	}
}

// Handle ForInStmt in ETF mode
fn (mut g CoreGen) core_for_in_chain_etf(stmt ast.ForInStmt, stmts []ast.Stmt, next_idx int) {
	base_accs := g.core_find_loop_accumulators(stmt.stmts)
	has_index := !stmt.is_range && stmt.key_var.len > 0 && stmt.key_var != '_'

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
			g.core_body_chain(stmts, next_idx, false)
			g.end_let()
			g.end_let()
		} else {
			new_var := g.next_core_var(all_accs[0])
			g.begin_let(new_var)
			g.core_for_foldl(stmt, all_accs, init_vals, '')
			g.mid_let()
			g.core_body_chain(stmts, next_idx, false)
			g.end_let()
		}
	} else {
		g.begin_seq()
		g.core_for_in(stmt)
		g.mid_seq()
		g.core_body_chain(stmts, next_idx, false)
		g.end_seq()
	}
}

// Handle ForInStmt in text mode
fn (mut g CoreGen) core_for_in_chain_text(stmt ast.ForInStmt, stmts []ast.Stmt, next_idx int, write_indent bool) {
	base_accs := g.core_find_loop_accumulators(stmt.stmts)
	has_index := !stmt.is_range && stmt.key_var.len > 0 && stmt.key_var != '_'

	if base_accs.len > 0 {
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
			g.core_for_in_with_index_text(stmt, base_accs, all_accs, init_vals, index_var, stmts, next_idx)
		} else {
			g.core_for_in_no_index_text(all_accs, stmt, init_vals, stmts, next_idx)
		}
	} else {
		if write_indent {
			g.write_indent_core()
		}
		g.out.write_string('do  ')
		g.core_for_in(stmt)
		g.out.writeln('')
		g.indent++
		g.core_body_chain(stmts, next_idx, true)
		g.indent--
	}
}

// Handle ForInStmt with index in text mode
fn (mut g CoreGen) core_for_in_with_index_text(stmt ast.ForInStmt, base_accs []string, all_accs []string, init_vals []string, index_var string, stmts []ast.Stmt, next_idx int) {
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
	g.core_body_chain(stmts, next_idx, false)
	g.indent -= 2
}

// Handle ForInStmt without index in text mode
fn (mut g CoreGen) core_for_in_no_index_text(all_accs []string, stmt ast.ForInStmt, init_vals []string, stmts []ast.Stmt, next_idx int) {
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
	g.core_body_chain(stmts, next_idx, false)
	g.indent--
}

// core_let_chain generates let bindings from an assignment statement,
// then continues with the rest of the body.
fn (mut g CoreGen) core_let_chain(node ast.AssignStmt, stmts []ast.Stmt, next_idx int, write_indent bool) {
	if node.left.len == 0 {
		g.core_body_chain(stmts, next_idx, write_indent)
		return
	}

	if g.etf_mode {
		g.core_let_chain_etf(node, stmts, next_idx)
	} else {
		g.core_let_chain_text(node, stmts, next_idx, write_indent)
	}
}

fn (mut g CoreGen) core_let_chain_etf(node ast.AssignStmt, stmts []ast.Stmt, next_idx int) {
	// ETF mode: use let/seq helpers (no indentation management)
	for i, left in node.left {
		is_last := i == node.left.len - 1
		rhs_idx := if i < node.right.len { i } else { 0 }

		if left is ast.Ident {
			g.core_let_chain_ident_etf(left, node.right[rhs_idx], is_last, stmts, next_idx)
		} else if left is ast.IndexExpr {
			g.core_let_chain_index_etf(left, node.right[rhs_idx], is_last, stmts, next_idx)
		} else {
			g.core_let_chain_other_etf(node.right[rhs_idx], is_last, stmts, next_idx)
		}
	}
}

fn (mut g CoreGen) core_let_chain_ident_etf(left ast.Ident, rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int) {
	if left.name == '_' {
		// Discard: seq(EXPR, REST)
		g.begin_seq()
		g.core_expr(rhs)
		g.mid_seq()
		if is_last {
			g.core_body_chain(stmts, next_idx, false)
		} else {
			g.emit_atom('ok')
		}
		g.end_seq()
		return
	}

	// Identity binding optimization
	if rhs is ast.Ident {
		if rhs.name in g.var_map {
			g.var_map[left.name] = g.var_map[rhs.name]
			if is_last {
				g.core_body_chain(stmts, next_idx, false)
			}
			return
		}
	}

	old_var_name := if left.name in g.var_map { g.var_map[left.name] } else { '' }
	var_name := g.next_core_var(left.name)

	g.begin_let(var_name)
	if old_var_name.len > 0 {
		g.var_map[left.name] = old_var_name
	}
	g.core_expr(rhs)
	g.var_map[left.name] = var_name
	g.mid_let()

	if is_last {
		g.core_body_chain(stmts, next_idx, false)
	}
	g.end_let()
}

fn (mut g CoreGen) core_let_chain_index_etf(left ast.IndexExpr, rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int) {
	if left.left is ast.Ident {
		map_name := (left.left as ast.Ident).name
		map_type_sym := g.table.sym(left.left_type)
		if map_type_sym.kind == .map || map_type_sym.name.starts_with('map[') {
			// Map mutation: m[k] = v -> let <NewMap> = maps:put(K, V, OldMap) in REST
			old_var := g.core_var(map_name)
			new_var := g.next_core_var(map_name)
			g.begin_let(new_var)
			g.begin_call('maps', 'put')
			g.core_expr(left.index)
			g.emit_sep()
			g.core_expr(rhs)
			g.emit_sep()
			g.emit_var(old_var)
			g.end_call()
			g.mid_let()
			if is_last {
				g.core_body_chain(stmts, next_idx, false)
			}
			g.end_let()
			return
		}
	}

	// Non-map index or non-ident left - side effect
	g.core_let_chain_other_etf(rhs, is_last, stmts, next_idx)
}

fn (mut g CoreGen) core_let_chain_other_etf(rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int) {
	g.begin_seq()
	g.core_expr(rhs)
	g.mid_seq()
	if is_last {
		g.core_body_chain(stmts, next_idx, false)
	} else {
		g.emit_atom('ok')
	}
	g.end_seq()
}

fn (mut g CoreGen) core_let_chain_text(node ast.AssignStmt, stmts []ast.Stmt, next_idx int, write_indent bool) {
	// Text mode: manual indentation management
	mut let_count := 0
	mut needs_indent := write_indent

	for i, left in node.left {
		is_last := i == node.left.len - 1
		rhs_idx := if i < node.right.len { i } else { 0 }

		if left is ast.Ident {
			result := g.core_let_chain_ident_text(left, node.right[rhs_idx], is_last, stmts, next_idx, needs_indent)
			let_count += result.let_count
			needs_indent = result.needs_indent
		} else if left is ast.IndexExpr {
			result := g.core_let_chain_index_text(left, node.right[rhs_idx], is_last, stmts, next_idx, needs_indent)
			let_count += result.let_count
			needs_indent = result.needs_indent
		} else {
			needs_indent = g.core_let_chain_other_text(node.right[rhs_idx], is_last, stmts, next_idx, needs_indent)
		}
	}

	// Unwind indent for each let binding we added
	for _ in 0 .. let_count {
		g.indent--
	}
}

struct LetChainResult {
	let_count     int
	needs_indent  bool
}

fn (mut g CoreGen) core_let_chain_ident_text(left ast.Ident, rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int, needs_indent bool) LetChainResult {
	mut ni := needs_indent

	if left.name == '_' {
		// Discard: do EXPR REST
		if ni {
			g.write_indent_core()
			ni = false
		}
		g.out.write_string('do  ')
		g.core_expr(rhs)
		g.out.writeln('')
		g.indent++
		if is_last {
			g.core_body_chain(stmts, next_idx, true)
		}
		g.indent--
		return LetChainResult{0, ni}
	}

	// Identity binding optimization
	if rhs is ast.Ident {
		if rhs.name in g.var_map {
			g.var_map[left.name] = g.var_map[rhs.name]
			if is_last {
				g.core_body_chain(stmts, next_idx, ni)
			}
			return LetChainResult{0, ni}
		}
	}

	old_var_name := if left.name in g.var_map { g.var_map[left.name] } else { '' }
	var_name := g.next_core_var(left.name)

	if ni {
		g.write_indent_core()
		ni = false
	}
	g.out.write_string('let <${var_name}> =')
	g.out.writeln('')
	g.indent++
	g.write_indent_core()
	if old_var_name.len > 0 {
		g.var_map[left.name] = old_var_name
	}
	g.core_expr(rhs)
	g.var_map[left.name] = var_name
	g.out.writeln('')
	g.indent--
	g.write_indent_core()
	g.out.write_string('in  ')
	g.indent++

	if is_last {
		g.core_body_chain(stmts, next_idx, false)
	}
	return LetChainResult{1, ni}
}

fn (mut g CoreGen) core_let_chain_index_text(left ast.IndexExpr, rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int, needs_indent bool) LetChainResult {
	mut ni := needs_indent

	if left.left is ast.Ident {
		map_name := (left.left as ast.Ident).name
		map_type_sym := g.table.sym(left.left_type)
		if map_type_sym.kind == .map || map_type_sym.name.starts_with('map[') {
			// Map mutation: m[k] = v -> let <NewMap> = maps:put(K, V, OldMap) in REST
			old_var := g.core_var(map_name)
			new_var := g.next_core_var(map_name)
			if ni {
				g.write_indent_core()
				ni = false
			}
			g.out.write_string('let <${new_var}> =')
			g.out.writeln('')
			g.indent++
			g.write_indent_core()
			g.out.write_string("call 'maps':'put'(")
			g.core_expr(left.index)
			g.out.write_string(', ')
			g.core_expr(rhs)
			g.out.write_string(', ${old_var})')
			g.out.writeln('')
			g.indent--
			g.write_indent_core()
			g.out.write_string('in  ')
			g.indent++
			if is_last {
				g.core_body_chain(stmts, next_idx, false)
			}
			return LetChainResult{1, ni}
		}
	}

	// Non-map index - side effect
	ni2 := g.core_let_chain_other_text(rhs, is_last, stmts, next_idx, ni)
	return LetChainResult{0, ni2}
}

fn (mut g CoreGen) core_let_chain_other_text(rhs ast.Expr, is_last bool, stmts []ast.Stmt, next_idx int, needs_indent bool) bool {
	mut ni := needs_indent
	if ni {
		g.write_indent_core()
		ni = false
	}
	g.out.write_string('do  ')
	g.core_expr(rhs)
	g.out.writeln('')
	g.indent++
	if is_last {
		g.core_body_chain(stmts, next_idx, true)
	}
	g.indent--
	return ni
}

// core_tail_stmt generates a statement in tail (return) position.
fn (mut g CoreGen) core_tail_stmt(stmt ast.Stmt, write_indent bool) {
	if g.etf_mode {
		g.core_tail_stmt_etf(stmt)
	} else {
		g.core_tail_stmt_text(stmt, write_indent)
	}
}

fn (mut g CoreGen) core_tail_stmt_etf(stmt ast.Stmt) {
	// ETF mode: no indentation, use helpers
	match stmt {
		ast.Return {
			g.core_tail_return_etf(stmt)
		}
		ast.ExprStmt {
			g.core_expr(stmt.expr)
		}
		ast.AssignStmt {
			g.core_tail_assign_etf(stmt)
		}
		else {
			g.emit_atom('ok')
		}
	}
}

fn (mut g CoreGen) core_tail_return_etf(stmt ast.Return) {
	if stmt.exprs.len > 0 {
		g.core_expr(stmt.exprs[0])
	} else {
		g.emit_atom('ok')
	}
}

fn (mut g CoreGen) core_tail_assign_etf(stmt ast.AssignStmt) {
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

fn (mut g CoreGen) core_tail_stmt_text(stmt ast.Stmt, write_indent bool) {
	// Text mode: with indentation
	match stmt {
		ast.Return {
			g.core_tail_return_text(stmt, write_indent)
		}
		ast.ExprStmt {
			g.core_tail_expr_text(stmt, write_indent)
		}
		ast.AssignStmt {
			g.core_tail_assign_text(stmt, write_indent)
		}
		else {
			if write_indent {
				g.write_indent_core()
			}
			g.out.writeln("'ok'")
		}
	}
}

fn (mut g CoreGen) core_tail_return_text(stmt ast.Return, write_indent bool) {
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

fn (mut g CoreGen) core_tail_expr_text(stmt ast.ExprStmt, write_indent bool) {
	if write_indent {
		g.write_indent_core()
	}
	g.core_expr(stmt.expr)
	g.out.writeln('')
}

fn (mut g CoreGen) core_tail_assign_text(stmt ast.AssignStmt, write_indent bool) {
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
	if g.etf_mode {
		g.core_for_foldl_etf(node, accumulators, init_vals, index_var)
	} else {
		g.core_for_foldl_text(node, accumulators, init_vals, index_var)
	}
}

// ETF mode: foldl-based for-in loop
fn (mut g CoreGen) core_for_foldl_etf(node ast.ForInStmt, accumulators []string, init_vals []string, index_var string) {
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()
	seq_temp := g.new_temp()

	mut acc_in := []string{}
	mut acc_out := []string{}
	for acc in accumulators {
		acc_in << '${acc.capitalize()}Acc'
		g.temp_counter++
		acc_out << '${acc.capitalize()}Out_${g.temp_counter}'
	}

	g.core_foldl_fun_etf(node, loop_var, fun_temp, accumulators, acc_in, acc_out, index_var)
	g.core_foldl_call_etf(node, fun_temp, seq_temp, accumulators, init_vals)
}

// ETF: define foldl function
fn (mut g CoreGen) core_foldl_fun_etf(node ast.ForInStmt, loop_var string, fun_temp string, accumulators []string, acc_in []string, acc_out []string, index_var string) {
	g.begin_let(fun_temp)
	if accumulators.len == 1 {
		g.begin_fun([loop_var, acc_in[0]])
	} else {
		acc_tuple := g.new_temp()
		g.begin_fun([loop_var, acc_tuple])
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

	g.core_fold_body(node.stmts, accumulators, acc_in, acc_out, index_var)

	if accumulators.len > 1 {
		for _ in acc_in {
			g.end_let()
		}
	}
	g.end_fun()
	g.mid_let()
}

// ETF: call foldl
fn (mut g CoreGen) core_foldl_call_etf(node ast.ForInStmt, fun_temp string, seq_temp string, accumulators []string, init_vals []string) {
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

	if node.is_range {
		g.end_let()
	}
	g.end_let()
}

// Text mode: foldl-based for-in loop
fn (mut g CoreGen) core_for_foldl_text(node ast.ForInStmt, accumulators []string, init_vals []string, index_var string) {
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()
	seq_temp := g.new_temp()

	mut acc_in := []string{}
	mut acc_out := []string{}
	for acc in accumulators {
		acc_in << '${acc.capitalize()}Acc'
		g.temp_counter++
		acc_out << '${acc.capitalize()}Out_${g.temp_counter}'
	}

	g.core_foldl_fun_text(node, loop_var, fun_temp, accumulators, acc_in, acc_out, index_var)
	g.core_foldl_call_text(node, fun_temp, seq_temp, accumulators, init_vals)
}

// Text: define foldl function
fn (mut g CoreGen) core_foldl_fun_text(node ast.ForInStmt, loop_var string, fun_temp string, accumulators []string, acc_in []string, acc_out []string, index_var string) {
	g.out.write_string('let <${fun_temp}> =')
	g.out.writeln('')
	g.indent++
	g.write_indent_core()
	if accumulators.len == 1 {
		g.out.writeln('fun (${loop_var}, ${acc_in[0]}) ->')
		g.indent++
	} else {
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

	g.core_fold_body(node.stmts, accumulators, acc_in, acc_out, index_var)

	g.indent -= 2
	g.write_indent_core()
	g.out.write_string('in  ')
}

// Text: call foldl
fn (mut g CoreGen) core_foldl_call_text(node ast.ForInStmt, fun_temp string, seq_temp string, accumulators []string, init_vals []string) {
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
		g.core_fold_body_etf(stmts, accumulators, acc_out, index_var)
	} else {
		g.core_fold_body_text(stmts, accumulators, acc_out, index_var)
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

fn (mut g CoreGen) core_fold_body_etf(stmts []ast.Stmt, accumulators []string, acc_out []string, index_var string) {
	// ETF mode: generate let bindings and seq for accumulator body
	// Track open wrappers (let/seq) so we can close them
	mut open_lets := 0
	mut open_seqs := 0

	for s_idx, stmt in stmts {
		_ = s_idx
		match stmt {
			ast.AssignStmt {
				open_lets += g.core_fold_assign_etf(stmt, accumulators, acc_out)
			}
			ast.ExprStmt {
				lets := g.core_fold_expr_stmt_etf(stmt, accumulators, acc_out)
				if lets >= 0 {
					open_lets += lets
				} else {
					// Regular side effect (lets == -1)
					g.begin_seq()
					g.core_expr(stmt.expr)
					g.mid_seq()
					open_seqs++
				}
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
	g.core_fold_return_accumulators_etf(accumulators, index_var)

	// Close open wrappers
	for _ in 0 .. open_seqs {
		g.end_seq()
	}
	for _ in 0 .. open_lets {
		g.end_let()
	}
}

fn (mut g CoreGen) core_fold_assign_etf(stmt ast.AssignStmt, accumulators []string, acc_out []string) int {
	mut open_lets := 0
	for j, left in stmt.left {
		if left is ast.Ident {
			acc_idx := accumulators.index(left.name)
			rhs_idx := if j < stmt.right.len { j } else { 0 }
			if acc_idx >= 0 {
				new_name := acc_out[acc_idx]
				g.begin_let(new_name)
				g.core_expr(stmt.right[rhs_idx])
				g.mid_let()
				g.var_map[left.name] = new_name
				open_lets++
			} else {
				var_name := g.next_core_var(left.name)
				g.begin_let(var_name)
				g.core_expr(stmt.right[rhs_idx])
				g.mid_let()
				open_lets++
			}
		}
	}
	return open_lets
}

fn (mut g CoreGen) core_fold_expr_stmt_etf(stmt ast.ExprStmt, accumulators []string, acc_out []string) int {
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
			return g.core_fold_if_phi_etf(stmt.expr, acc_modified, accumulators, acc_out)
		}
	}
	// Regular side effect - signal with -1
	return -1
}

fn (mut g CoreGen) core_fold_if_phi_etf(expr ast.IfExpr, acc_modified []string, accumulators []string, acc_out []string) int {
	mut open_lets := 0
	if acc_modified.len == 1 {
		mvar := acc_modified[0]
		acc_idx := accumulators.index(mvar)
		if acc_idx >= 0 {
			old_val := g.var_map[mvar]
			new_name := acc_out[acc_idx]
			g.begin_let(new_name)
			g.core_phi_if_branches(expr.branches, 0, mvar, old_val)
			g.mid_let()
			g.var_map[mvar] = new_name
			open_lets++
		}
	} else {
		tuple_temp := g.new_temp()
		g.begin_let(tuple_temp)
		g.core_phi_multi_if(expr, acc_modified, accumulators)
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
	return open_lets
}

fn (mut g CoreGen) core_fold_return_accumulators_etf(accumulators []string, index_var string) {
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
}

fn (mut g CoreGen) core_fold_body_text(stmts []ast.Stmt, accumulators []string, acc_out []string, index_var string) {
	// Text mode: manual indentation management
	for s_idx, stmt in stmts {
		_ = s_idx
		match stmt {
			ast.AssignStmt {
				g.core_fold_assign_text(stmt, accumulators, acc_out)
			}
			ast.ExprStmt {
				if !g.core_fold_expr_stmt_text(stmt, accumulators, acc_out) {
					// Regular side effect
					g.write_indent_core()
					g.out.write_string('do  ')
					g.core_expr(stmt.expr)
					g.out.writeln('')
					g.indent++
				}
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
	g.core_fold_return_accumulators_text(accumulators, index_var)

	// Unwind indent for side effects (do statements)
	g.core_fold_unwind_text(stmts, accumulators)
}

fn (mut g CoreGen) core_fold_assign_text(stmt ast.AssignStmt, accumulators []string, acc_out []string) {
	for j, left in stmt.left {
		if left is ast.Ident {
			acc_idx := accumulators.index(left.name)
			rhs_idx := if j < stmt.right.len { j } else { 0 }
			if acc_idx >= 0 {
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
				g.var_map[left.name] = new_name
				_ = old_name
			} else {
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

fn (mut g CoreGen) core_fold_expr_stmt_text(stmt ast.ExprStmt, accumulators []string, acc_out []string) bool {
	// Check if this is an if-block that modifies accumulators
	// Returns true if handled as phi-node, false if regular side effect
	if stmt.expr is ast.IfExpr {
		modified := g.core_find_if_modified_vars(stmt.expr)
		mut acc_modified := []string{}
		for m in modified {
			if accumulators.index(m) >= 0 {
				acc_modified << m
			}
		}
		if acc_modified.len > 0 {
			g.core_fold_if_phi_text(stmt.expr, acc_modified, accumulators, acc_out)
			return true
		}
	}
	return false
}

fn (mut g CoreGen) core_fold_if_phi_text(expr ast.IfExpr, acc_modified []string, accumulators []string, acc_out []string) {
	if acc_modified.len == 1 {
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
			g.core_phi_if_branches(expr.branches, 0, mvar, old_val)
			g.out.writeln('')
			g.indent--
			g.write_indent_core()
			g.out.write_string('in  ')
			g.var_map[mvar] = new_name
		}
	} else {
		tuple_temp := g.new_temp()
		g.write_indent_core()
		g.out.write_string('let <${tuple_temp}> =')
		g.out.writeln('')
		g.indent++
		g.write_indent_core()
		g.core_phi_multi_if(expr, acc_modified, accumulators)
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
}

fn (mut g CoreGen) core_fold_return_accumulators_text(accumulators []string, index_var string) {
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
}

fn (mut g CoreGen) core_fold_unwind_text(stmts []ast.Stmt, accumulators []string) {
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
	if g.etf_mode {
		g.core_for_collection_foreach_etf(node)
	} else {
		g.core_for_collection_foreach_text(node)
	}
}

// ETF mode: for-in collection iteration
fn (mut g CoreGen) core_for_collection_foreach_etf(node ast.ForInStmt) {
	has_index := node.key_var.len > 0 && node.key_var != '_'
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()

	if has_index {
		g.core_foreach_indexed_etf(node, loop_var, fun_temp)
	} else {
		g.core_foreach_simple_etf(node, loop_var, fun_temp)
	}
}

// ETF: indexed foreach (foldl with counter)
fn (mut g CoreGen) core_foreach_indexed_etf(node ast.ForInStmt, loop_var string, fun_temp string) {
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
		g.core_foreach_body_etf(node.stmts, idx_var)
	}
	g.end_fun()
	g.mid_let()

	g.begin_call('lists', 'foldl')
	g.emit_var(fun_temp)
	g.emit_sep()
	g.emit_int('0')
	g.emit_sep()
	g.core_expr(node.cond)
	g.end_call()

	g.end_let()
}

// ETF: body of indexed foreach
fn (mut g CoreGen) core_foreach_body_etf(stmts []ast.Stmt, idx_var string) {
	for stmt in stmts {
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
	for _ in stmts {
		g.end_seq()
	}
}

// ETF: simple foreach (no index)
fn (mut g CoreGen) core_foreach_simple_etf(node ast.ForInStmt, loop_var string, fun_temp string) {
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

// Text mode: for-in collection iteration
fn (mut g CoreGen) core_for_collection_foreach_text(node ast.ForInStmt) {
	has_index := node.key_var.len > 0 && node.key_var != '_'
	loop_var := g.core_var(node.val_var)
	fun_temp := g.new_temp()

	if has_index {
		g.core_foreach_indexed_text(node, loop_var, fun_temp)
	} else {
		g.core_foreach_simple_text(node, loop_var, fun_temp)
	}
}

// Text: indexed foreach
fn (mut g CoreGen) core_foreach_indexed_text(node ast.ForInStmt, loop_var string, fun_temp string) {
	idx_var := g.core_var(node.key_var)
	pair_temp := g.new_temp()
	_ = pair_temp

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
		g.core_foreach_body_text(node.stmts, idx_var)
	}

	g.indent -= 2
	g.write_indent_core()
	g.out.write_string("in  call 'lists':'foldl'(${fun_temp}, 0, ")
	g.core_expr(node.cond)
	g.out.write_string(')')
}

// Text: body of indexed foreach
fn (mut g CoreGen) core_foreach_body_text(stmts []ast.Stmt, idx_var string) {
	for stmt in stmts {
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
	for _ in stmts {
		g.indent--
	}
}

// Text: simple foreach (no index)
fn (mut g CoreGen) core_foreach_simple_text(node ast.ForInStmt, loop_var string, fun_temp string) {
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

	// Handle IfGuardExpr: if val := expr { return val } rest...
	if branch.cond is ast.IfGuardExpr {
		g.core_early_return_guard(branch.cond, branch, stmts, next_idx)
		return
	}

	if g.etf_mode {
		g.core_early_return_case_etf(branch, stmts, next_idx)
	} else {
		g.core_early_return_case_text(branch, stmts, next_idx, write_indent)
	}
}

fn (mut g CoreGen) core_early_return_case_etf(branch ast.IfBranch, stmts []ast.Stmt, next_idx int) {
	// case COND of <'true'> -> RETURN_VAL ; <'false'> -> REST end
	g.begin_case()
	g.core_expr(branch.cond)
	g.mid_case_clauses()

	g.core_early_return_case_etf_true(branch)
	g.clause_sep()
	g.core_early_return_case_etf_false(stmts, next_idx)

	g.end_case()
}

fn (mut g CoreGen) core_early_return_case_etf_true(branch ast.IfBranch) {
	// true clause
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_branch_return_val(branch)
	g.end_clause()
}

fn (mut g CoreGen) core_early_return_case_etf_false(stmts []ast.Stmt, next_idx int) {
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
}

fn (mut g CoreGen) core_early_return_case_text(branch ast.IfBranch, stmts []ast.Stmt, next_idx int, write_indent bool) {
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

	g.core_early_return_case_text_true(branch)
	g.core_early_return_case_text_false(stmts, next_idx)

	g.indent--
	g.write_indent_core()
	g.out.writeln('end')
}

fn (mut g CoreGen) core_early_return_case_text_true(branch ast.IfBranch) {
	// True branch - the early return value
	g.write_indent_core()
	g.out.write_string("<'true'> when 'true' -> ")
	g.core_branch_return_val(branch)
	g.out.writeln('')
}

fn (mut g CoreGen) core_early_return_case_text_false(stmts []ast.Stmt, next_idx int) {
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
		g.core_early_return_guard_etf(guard, branch, stmts, next_idx, var_names, is_map_lookup)
	} else {
		g.core_early_return_guard_text(guard, branch, stmts, next_idx, var_names, is_map_lookup)
	}
}

fn (mut g CoreGen) core_early_return_guard_etf(guard ast.IfGuardExpr, branch ast.IfBranch, stmts []ast.Stmt, next_idx int, var_names []string, is_map_lookup bool) {
	// ETF mode: case EXPR of <{ok, Val}> -> ... ; <_> -> REST end
	g.begin_case()

	g.core_early_return_guard_etf_expr(guard, is_map_lookup)
	g.mid_case_clauses()
	g.core_early_return_guard_etf_success(branch, var_names)
	g.clause_sep()
	g.core_early_return_guard_etf_failure(stmts, next_idx)

	g.end_case()
}

fn (mut g CoreGen) core_early_return_guard_etf_expr(guard ast.IfGuardExpr, is_map_lookup bool) {
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
}

fn (mut g CoreGen) core_early_return_guard_etf_success(branch ast.IfBranch, var_names []string) {
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
}

fn (mut g CoreGen) core_early_return_guard_etf_failure(stmts []ast.Stmt, next_idx int) {
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
}

fn (mut g CoreGen) core_early_return_guard_text(guard ast.IfGuardExpr, branch ast.IfBranch, stmts []ast.Stmt, next_idx int, var_names []string, is_map_lookup bool) {
	g.core_early_return_guard_text_expr(guard, is_map_lookup)
	g.indent++
	g.core_early_return_guard_text_success(branch, var_names)
	g.core_early_return_guard_text_failure(stmts, next_idx)
	g.indent--
	g.write_indent_core()
	g.out.writeln('end')
}

fn (mut g CoreGen) core_early_return_guard_text_expr(guard ast.IfGuardExpr, is_map_lookup bool) {
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
}

fn (mut g CoreGen) core_early_return_guard_text_success(branch ast.IfBranch, var_names []string) {
	// Success branch: {ok, Val} -> return val
	g.write_indent_core()
	if var_names.len == 1 {
		g.out.write_string("<{'ok', ${var_names[0]}}> when 'true' -> ")
	} else {
		g.out.write_string("<{'ok', _}> when 'true' -> ")
	}
	g.core_branch_return_val(branch)
	g.out.writeln('')
}

fn (mut g CoreGen) core_early_return_guard_text_failure(stmts []ast.Stmt, next_idx int) {
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

	last_assign_idx := g.core_find_last_assign(branch.stmts, target_var)

	if g.etf_mode {
		g.core_phi_branch_body_etf(branch, target_var, old_val, last_assign_idx)
	} else {
		g.core_phi_branch_body_text(branch, target_var, old_val, last_assign_idx)
	}
}

// Find last assignment to target variable in phi branch
fn (g CoreGen) core_find_last_assign(stmts []ast.Stmt, target_var string) int {
	mut last_assign_idx := -1
	for i, stmt in stmts {
		if stmt is ast.AssignStmt {
			for left in stmt.left {
				if left is ast.Ident && left.name == target_var {
					last_assign_idx = i
				}
			}
		}
	}
	return last_assign_idx
}

// ETF: phi branch body
fn (mut g CoreGen) core_phi_branch_body_etf(branch ast.IfBranch, target_var string, old_val string, last_assign_idx int) {
	if last_assign_idx < 0 {
		g.core_phi_unmodified_etf(branch, old_val)
		return
	}

	g.core_phi_gen_stmts_etf(branch.stmts, target_var, last_assign_idx)
	g.core_phi_close_wrappers_etf(branch.stmts, target_var, last_assign_idx)
}

// ETF: unmodified target variable
fn (mut g CoreGen) core_phi_unmodified_etf(branch ast.IfBranch, old_val string) {
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
}

// ETF: generate statements up to last assignment
fn (mut g CoreGen) core_phi_gen_stmts_etf(stmts []ast.Stmt, target_var string, last_assign_idx int) {
	for i, stmt in stmts {
		if i > last_assign_idx {
			break
		}
		if stmt is ast.AssignStmt {
			for j, left in stmt.left {
				if left is ast.Ident && left.name == target_var {
					rhs_idx := if j < stmt.right.len { j } else { 0 }
					if i < last_assign_idx {
						g.begin_seq()
						g.core_expr(stmt.right[rhs_idx])
						g.mid_seq()
					} else {
						g.core_expr(stmt.right[rhs_idx])
					}
				} else if left is ast.Ident {
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
}

// ETF: close wrappers for non-final assignments
fn (mut g CoreGen) core_phi_close_wrappers_etf(stmts []ast.Stmt, target_var string, last_assign_idx int) {
	for i, stmt in stmts {
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
}

// Text: phi branch body
fn (mut g CoreGen) core_phi_branch_body_text(branch ast.IfBranch, target_var string, old_val string, last_assign_idx int) {
	if last_assign_idx < 0 {
		g.core_phi_unmodified_text(branch, old_val)
		return
	}

	g.core_phi_gen_stmts_text(branch.stmts, target_var, last_assign_idx)
}

// Text: unmodified target variable
fn (mut g CoreGen) core_phi_unmodified_text(branch ast.IfBranch, old_val string) {
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
}

// Text: generate statements up to last assignment
fn (mut g CoreGen) core_phi_gen_stmts_text(stmts []ast.Stmt, target_var string, last_assign_idx int) {
	for i, stmt in stmts {
		if i > last_assign_idx {
			break
		}
		if stmt is ast.AssignStmt {
			for j, left in stmt.left {
				if left is ast.Ident && left.name == target_var {
					rhs_idx := if j < stmt.right.len { j } else { 0 }
					if i < last_assign_idx {
						g.write_core('do  ')
						g.core_expr(stmt.right[rhs_idx])
						g.write_core(' ')
					} else {
						g.core_expr(stmt.right[rhs_idx])
					}
				} else if left is ast.Ident {
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
		g.core_phi_emit_default_tuple(target_vars)
		return
	}

	branch := branches[idx]
	is_last := idx == branches.len - 1
	is_else := is_last && (branch.cond is ast.NodeError || branch.cond is ast.EmptyExpr)

	if g.etf_mode {
		g.core_phi_multi_branch_etf(branch, branches, idx, target_vars, is_else)
	} else {
		g.core_phi_multi_branch_text(branch, branches, idx, target_vars, is_else)
	}
}

fn (mut g CoreGen) core_phi_emit_default_tuple(target_vars []string) {
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
}

fn (mut g CoreGen) core_phi_multi_branch_etf(branch ast.IfBranch, branches []ast.IfBranch, idx int, target_vars []string, is_else bool) {
	if is_else {
		g.core_phi_multi_body(branch, target_vars)
	} else {
		g.core_phi_multi_branch_etf_case(branch, branches, idx, target_vars)
	}
}

fn (mut g CoreGen) core_phi_multi_branch_etf_case(branch ast.IfBranch, branches []ast.IfBranch, idx int, target_vars []string) {
	g.begin_case()
	g.core_expr(branch.cond)
	g.mid_case_clauses()

	// true clause
	g.core_phi_multi_branch_etf_true(branch, target_vars)
	g.clause_sep()

	// false clause
	g.core_phi_multi_branch_etf_false(branches, idx, target_vars)
	g.end_case()
}

fn (mut g CoreGen) core_phi_multi_branch_etf_true(branch ast.IfBranch, target_vars []string) {
	g.begin_clause()
	g.emit_atom('true')
	g.mid_clause_guard()
	g.emit_true_guard()
	g.mid_clause_body()
	g.core_phi_multi_body(branch, target_vars)
	g.end_clause()
}

fn (mut g CoreGen) core_phi_multi_branch_etf_false(branches []ast.IfBranch, idx int, target_vars []string) {
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
}

fn (mut g CoreGen) core_phi_multi_branch_text(branch ast.IfBranch, branches []ast.IfBranch, idx int, target_vars []string, is_else bool) {
	if is_else {
		g.core_phi_multi_body(branch, target_vars)
	} else {
		g.core_phi_multi_branch_text_case(branch, branches, idx, target_vars)
	}
}

fn (mut g CoreGen) core_phi_multi_branch_text_case(branch ast.IfBranch, branches []ast.IfBranch, idx int, target_vars []string) {
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

// core_phi_multi_body generates a tuple of values for a branch body,
// one value per target variable (RHS if assigned, old value if not).
fn (mut g CoreGen) core_phi_multi_body(branch ast.IfBranch, target_vars []string) {
	assign_map, max_assign := g.core_phi_build_assign_map(branch, target_vars)

	if g.etf_mode {
		g.core_phi_multi_body_etf(branch, target_vars, assign_map, max_assign)
	} else {
		g.core_phi_multi_body_text(branch, target_vars, assign_map, max_assign)
	}
}

fn (mut g CoreGen) core_phi_build_assign_map(branch ast.IfBranch, target_vars []string) (map[string]int, int) {
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

	return assign_map, max_assign
}

fn (mut g CoreGen) core_phi_multi_body_etf(branch ast.IfBranch, target_vars []string, assign_map map[string]int, max_assign int) {
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
	g.core_phi_multi_body_tuple_etf(branch, target_vars, assign_map)

	// Close seq wrappers
	for _ in 0 .. seq_count {
		g.end_seq()
	}
}

fn (mut g CoreGen) core_phi_multi_body_tuple_etf(branch ast.IfBranch, target_vars []string, assign_map map[string]int) {
	g.begin_tuple()
	for j, tv in target_vars {
		if j > 0 {
			g.emit_sep()
		}
		g.core_phi_emit_var_value(branch, tv, assign_map, true)
	}
	g.end_tuple()
}

fn (mut g CoreGen) core_phi_multi_body_text(branch ast.IfBranch, target_vars []string, assign_map map[string]int, max_assign int) {
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
	g.core_phi_multi_body_tuple_text(branch, target_vars, assign_map)
}

fn (mut g CoreGen) core_phi_multi_body_tuple_text(branch ast.IfBranch, target_vars []string, assign_map map[string]int) {
	g.write_core('{')
	for j, tv in target_vars {
		if j > 0 {
			g.write_core(', ')
		}
		g.core_phi_emit_var_value(branch, tv, assign_map, false)
	}
	g.write_core('}')
}

fn (mut g CoreGen) core_phi_emit_var_value(branch ast.IfBranch, tv string, assign_map map[string]int, is_etf bool) {
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
				if is_etf {
					g.emit_var(g.core_var(tv))
				} else {
					g.write_core(g.core_var(tv))
				}
			}
		} else {
			if is_etf {
				g.emit_var(g.core_var(tv))
			} else {
				g.write_core(g.core_var(tv))
			}
		}
	} else {
		// Not modified in this branch - return old value
		if is_etf {
			g.emit_var(g.core_var(tv))
		} else {
			g.write_core(g.core_var(tv))
		}
	}
}

// core_while_loop generates a while loop using letrec.
fn (mut g CoreGen) core_while_loop(node ast.ForStmt, stmts []ast.Stmt, next_idx int) {
	base_accs := g.core_find_loop_accumulators(node.stmts)
	g.temp_counter++
	loop_name := 'loop_${g.temp_counter}'

	if base_accs.len == 0 {
		g.core_while_simple(node, stmts, next_idx, loop_name)
	} else {
		g.core_while_with_accs(node, stmts, next_idx, loop_name, base_accs)
	}
}

// While loop with no accumulators (simple side-effect loop)
fn (mut g CoreGen) core_while_simple(node ast.ForStmt, stmts []ast.Stmt, next_idx int, loop_name string) {
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
}

// While loop with accumulators
fn (mut g CoreGen) core_while_with_accs(node ast.ForStmt, stmts []ast.Stmt, next_idx int, loop_name string, base_accs []string) {
	arity := base_accs.len
	mut param_names := []string{}
	mut init_vals := []string{}
	for acc in base_accs {
		param_names << '${acc.capitalize()}Param'
		init_vals << g.core_var(acc)
	}

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

	g.core_while_letrec_def(node, loop_name, arity, param_names, base_accs, out_names)
	g.core_while_result_binding(stmts, next_idx, loop_name, arity, init_vals, base_accs, saved)
}

// Define letrec for while loop with accumulators
fn (mut g CoreGen) core_while_letrec_def(node ast.ForStmt, loop_name string, arity int, param_names []string, base_accs []string, out_names []string) {
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
}

// Bind result of while loop and continue
fn (mut g CoreGen) core_while_result_binding(stmts []ast.Stmt, next_idx int, loop_name string, arity int, init_vals []string, base_accs []string, saved map[string]string) {
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

// core_c_style_loop generates a C-style for loop using letrec.
fn (mut g CoreGen) core_c_style_loop(node ast.ForCStmt, stmts []ast.Stmt, next_idx int) {
	base_accs := g.core_find_loop_accumulators(node.stmts)
	g.temp_counter++
	loop_name := 'loop_${g.temp_counter}'

	loop_var_name, accs := g.core_c_loop_setup(node, base_accs)
	arity := accs.len + 1
	loop_var_param := '${loop_var_name.capitalize()}Param'

	param_names, init_vals := g.core_c_loop_params(node, accs, loop_var_param, loop_var_name)
	saved, out_names := g.core_c_loop_save_state(accs, loop_var_name)

	g.core_c_loop_letrec(node, loop_name, arity, param_names, accs, loop_var_name,
		loop_var_param, out_names)
	g.core_c_loop_result(stmts, next_idx, loop_name, arity, init_vals, accs, saved,
		loop_var_name, loop_var_param)
}

// Setup loop variable and accumulators for C-style loop
fn (mut g CoreGen) core_c_loop_setup(node ast.ForCStmt, base_accs []string) (string, []string) {
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
	return loop_var_name, accs
}

// Setup parameters and init values for C-style loop
fn (mut g CoreGen) core_c_loop_params(node ast.ForCStmt, accs []string, loop_var_param string, loop_var_name string) ([]string, []string) {
	mut param_names := [loop_var_param]
	mut init_vals := []string{}
	for acc in accs {
		param_names << '${acc.capitalize()}Param'
		init_vals << g.core_var(acc)
	}

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
	return param_names, init_vals
}

// Save var_map state before C-style loop
fn (mut g CoreGen) core_c_loop_save_state(accs []string, loop_var_name string) (map[string]string, []string) {
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
	return saved, out_names
}

// Define letrec for C-style loop
fn (mut g CoreGen) core_c_loop_letrec(node ast.ForCStmt, loop_name string, arity int, param_names []string, accs []string, loop_var_name string, loop_var_param string, out_names []string) {
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
}

// Bind result of C-style loop and continue
fn (mut g CoreGen) core_c_loop_result(stmts []ast.Stmt, next_idx int, loop_name string, arity int, init_vals []string, accs []string, saved map[string]string, loop_var_name string, loop_var_param string) {
	for acc in accs {
		if acc in saved {
			g.var_map[acc] = saved[acc]
		}
	}
	if loop_var_name in saved {
		g.var_map[loop_var_name] = saved[loop_var_name]
	}

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

	if accs.len > 0 {
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
	}

	g.core_body_chain(stmts, next_idx, false)

	if accs.len > 0 {
		for _ in accs {
			g.end_let()
		}
	}
	g.end_let()
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
	open_wrappers := g.core_c_loop_body_stmts(body_stmts, accumulators, out_names)
	g.core_c_loop_increment(loop_name, arity, loop_var_name, accumulators, inc_stmt, open_wrappers)
}

fn (mut g CoreGen) core_c_loop_body_stmts(body_stmts []ast.Stmt, accumulators []string, out_names []string) int {
	mut open_wrappers := 0
	for stmt in body_stmts {
		match stmt {
			ast.AssignStmt {
				open_wrappers += g.core_c_loop_assign_stmt(stmt, accumulators, out_names)
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
	return open_wrappers
}

fn (mut g CoreGen) core_c_loop_assign_stmt(stmt ast.AssignStmt, accumulators []string, out_names []string) int {
	mut count := 0
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
				count++
			} else {
				rhs_idx := if j < stmt.right.len { j } else { 0 }
				var_name := g.next_core_var(left.name)
				g.begin_let(var_name)
				g.core_expr(stmt.right[rhs_idx])
				g.mid_let()
				count++
			}
		}
	}
	return count
}

fn (mut g CoreGen) core_c_loop_increment(loop_name string, arity int, loop_var_name string, accumulators []string, inc_stmt ast.Stmt, open_wrappers int) {
	// Compute increment and recurse
	inc_temp := g.new_temp()
	g.begin_let(inc_temp)
	g.core_c_loop_inc_expr(loop_var_name, inc_stmt)
	g.mid_let()

	g.core_c_loop_tail_call(loop_name, arity, inc_temp, accumulators)
	g.end_let()

	g.core_c_loop_close_wrappers(open_wrappers)
}

fn (mut g CoreGen) core_c_loop_inc_expr(loop_var_name string, inc_stmt ast.Stmt) {
	if inc_stmt is ast.AssignStmt {
		inc_assign := inc_stmt as ast.AssignStmt
		if inc_assign.right.len > 0 {
			g.core_expr(inc_assign.right[0])
		} else {
			g.core_c_loop_default_inc(loop_var_name)
		}
	} else {
		g.core_c_loop_default_inc(loop_var_name)
	}
}

fn (mut g CoreGen) core_c_loop_default_inc(loop_var_name string) {
	g.begin_call('erlang', '+')
	g.emit_var(g.core_var(loop_var_name))
	g.emit_sep()
	g.emit_int('1')
	g.end_call()
}

fn (mut g CoreGen) core_c_loop_tail_call(loop_name string, arity int, inc_temp string, accumulators []string) {
	g.begin_apply(loop_name, arity)
	g.emit_var(inc_temp)
	for acc in accumulators {
		g.emit_sep()
		g.emit_var(g.core_var(acc))
	}
	g.end_apply()
}

fn (mut g CoreGen) core_c_loop_close_wrappers(open_wrappers int) {
	for _ in 0 .. open_wrappers {
		if g.etf_mode {
			g.write_core('}')
		}
	}
}
