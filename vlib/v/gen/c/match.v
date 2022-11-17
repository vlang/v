// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) need_tmp_var_in_match(node ast.MatchExpr) bool {
	if node.is_expr && node.return_type != ast.void_type && node.return_type != 0 {
		if g.table.sym(node.return_type).kind in [.sum_type, .multi_return]
			|| node.return_type.has_flag(.optional) || node.return_type.has_flag(.result) {
			return true
		}
		if g.table.final_sym(node.cond_type).kind == .enum_ && node.branches.len > 5 {
			return true
		}
		if g.need_tmp_var_in_expr(node.cond) {
			return true
		}
		for branch in node.branches {
			if branch.stmts.len > 1 {
				return true
			}
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if g.need_tmp_var_in_expr(stmt.expr) {
						return true
					}
				}
			}
		}
	}
	return false
}

fn (mut g Gen) match_expr(node ast.MatchExpr) {
	if node.cond_type == 0 {
		g.writeln('// match 0')
		return
	}
	need_tmp_var := g.need_tmp_var_in_match(node)
	is_expr := (node.is_expr && node.return_type != ast.void_type) || g.inside_ternary > 0

	mut cond_var := ''
	mut tmp_var := ''
	mut cur_line := ''
	if is_expr && !need_tmp_var {
		g.inside_ternary++
	}
	if is_expr {
		if node.return_type.has_flag(.optional) {
			old := g.inside_match_optional
			defer {
				g.inside_match_optional = old
			}
			g.inside_match_optional = true
		} else if node.return_type.has_flag(.result) {
			old := g.inside_match_result
			defer {
				g.inside_match_result = old
			}
			g.inside_match_result = true
		}
	}
	if node.cond in [ast.Ident, ast.SelectorExpr, ast.IntegerLiteral, ast.StringLiteral, ast.FloatLiteral] {
		cond_var = g.expr_string(node.cond)
	} else {
		line := if is_expr {
			g.empty_line = true
			g.go_before_stmt(0)
		} else {
			''
		}
		cond_var = g.new_tmp_var()
		g.write('${g.typ(node.cond_type)} ${cond_var} = ')
		g.expr(node.cond)
		g.writeln(';')
		g.set_current_pos_as_last_stmt_pos()
		g.write(line)
	}
	if need_tmp_var {
		g.empty_line = true
		cur_line = g.go_before_stmt(0).trim_left(' \t')
		tmp_var = g.new_tmp_var()
		g.writeln('${g.typ(node.return_type)} ${tmp_var} = ${g.type_default(node.return_type)};')
	}

	if is_expr && !need_tmp_var {
		// brackets needed otherwise '?' will apply to everything on the left
		g.write('(')
	}
	if node.is_sum_type {
		g.match_expr_sumtype(node, is_expr, cond_var, tmp_var)
	} else {
		cond_fsym := g.table.final_sym(node.cond_type)
		mut can_be_a_switch := true
		all_branches: for branch in node.branches {
			for expr in branch.exprs {
				match expr {
					ast.BoolLiteral, ast.IntegerLiteral, ast.CharLiteral, ast.EnumVal {
						continue
					}
					else {
						// ast.StringLiteral, ast.Ident, ast.RangeExpr can not used in switch cases in C
						// eprintln('>>>> node.cond: $node.cond | branch expr: ${typeof(expr)} | expr: $expr')
						can_be_a_switch = false
						break all_branches
					}
				}
			}
		}
		// eprintln('> can_be_a_switch: $can_be_a_switch')
		if can_be_a_switch && !is_expr && g.loop_depth == 0 && g.fn_decl != unsafe { nil }
			&& cond_fsym.is_int() {
			g.match_expr_switch(node, is_expr, cond_var, tmp_var, cond_fsym)
		} else if cond_fsym.kind == .enum_ && g.loop_depth == 0 && node.branches.len > 5
			&& g.fn_decl != unsafe { nil } {
			// do not optimize while in top-level
			g.match_expr_switch(node, is_expr, cond_var, tmp_var, cond_fsym)
		} else {
			g.match_expr_classic(node, is_expr, cond_var, tmp_var)
		}
	}
	g.set_current_pos_as_last_stmt_pos()
	g.write(cur_line)
	if need_tmp_var {
		g.write('${tmp_var}')
	}
	if is_expr && !need_tmp_var {
		g.write(')')
		g.decrement_inside_ternary()
	}
}

fn (mut g Gen) match_expr_sumtype(node ast.MatchExpr, is_expr bool, cond_var string, tmp_var string) {
	dot_or_ptr := if node.cond_type.is_ptr() { '->' } else { '.' }
	use_ternary := is_expr && tmp_var.len == 0
	cond_sym := g.table.sym(node.cond_type)
	for j, branch in node.branches {
		mut sumtype_index := 0
		// iterates through all types in sumtype branches
		for {
			g.aggregate_type_idx = sumtype_index
			is_last := j == node.branches.len - 1 && sumtype_index == branch.exprs.len - 1
			if branch.is_else || (use_ternary && is_last) {
				if use_ternary {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.writeln('else {')
				}
			} else {
				if j > 0 || sumtype_index > 0 {
					if use_ternary {
						g.write(' : ')
					} else {
						g.write_v_source_line_info(branch.pos)
						g.write('else ')
					}
				}
				if use_ternary {
					g.write('(')
				} else {
					if j == 0 && sumtype_index == 0 {
						g.empty_line = true
					}
					g.write_v_source_line_info(branch.pos)
					g.write('if (')
				}
				g.write(cond_var)
				cur_expr := unsafe { &branch.exprs[sumtype_index] }
				if cond_sym.kind == .sum_type {
					g.write('${dot_or_ptr}_typ == ')
					if cur_expr is ast.None {
						g.write('${ast.none_type.idx()} /* none */')
					} else {
						g.expr(cur_expr)
					}
				} else if cond_sym.kind == .interface_ {
					if cur_expr is ast.TypeNode {
						branch_sym := g.table.sym(g.unwrap_generic(cur_expr.typ))
						g.write('${dot_or_ptr}_typ == _${cond_sym.cname}_${branch_sym.cname}_index')
					} else if cur_expr is ast.None && cond_sym.idx == ast.error_type_idx {
						g.write('${dot_or_ptr}_typ == _IError_None___index')
					}
				}
				if use_ternary {
					g.write(')? ')
				} else {
					g.writeln(') {')
				}
			}
			if is_expr && tmp_var.len > 0 && g.table.sym(node.return_type).kind == .sum_type {
				g.expected_cast_type = node.return_type
			}
			g.stmts_with_tmp_var(branch.stmts, tmp_var)
			g.expected_cast_type = 0
			if g.inside_ternary == 0 {
				g.writeln('}')
				g.set_current_pos_as_last_stmt_pos()
			}
			sumtype_index++
			if branch.exprs.len == 0 || sumtype_index == branch.exprs.len {
				break
			}
		}
		// reset global field for next use
		g.aggregate_type_idx = 0
	}
}

fn (mut g Gen) match_expr_switch(node ast.MatchExpr, is_expr bool, cond_var string, tmp_var string, cond_fsym ast.TypeSymbol) {
	node_cond_type_unsigned := node.cond_type in [ast.u16_type, ast.u32_type, ast.u64_type]
	cname := '${cond_fsym.cname}__'

	mut covered_enum_cap := 0
	if cond_fsym.info is ast.Enum {
		covered_enum_cap = (cond_fsym.info as ast.Enum).vals.len
	}
	mut covered_enum := []string{cap: covered_enum_cap} // collects missing enum variant branches to avoid cstrict errors

	// A branch that has a RangeExpr condition, cannot be emitted as a switch case branch;
	// we will store each of them in range_branches, and then will handle them all in the default branch with if conditions:
	mut range_branches := []ast.MatchBranch{cap: node.branches.len}
	mut default_generated := false

	g.empty_line = true
	g.writeln('switch (${cond_var}) {')
	g.indent++
	for branch in node.branches {
		if branch.is_else {
			if cond_fsym.info is ast.Enum {
				for val in (cond_fsym.info as ast.Enum).vals {
					if val !in covered_enum {
						g.writeln('case ${cname}${val}:')
					}
				}
			}
			g.write('default: ')
			default_generated = true
			if range_branches.len > 0 {
				g.indent++
				for range_branch in range_branches {
					g.write('if (')
					for i, expr in range_branch.exprs {
						if i > 0 {
							g.write(' || ')
						}
						if expr is ast.RangeExpr {
							// if type is unsigned and low is 0, check is unneeded
							mut skip_low := false
							if expr.low is ast.IntegerLiteral {
								if node_cond_type_unsigned && expr.low.val == '0' {
									skip_low = true
								}
							}
							g.write('(')
							if !skip_low {
								g.write('${cond_var} >= ')
								g.expr(expr.low)
								g.write(' && ')
							}
							g.write('${cond_var} <= ')
							g.expr(expr.high)
							g.write(')')
						} else {
							g.write('${cond_var} == (')
							g.expr(expr)
							g.write(')')
						}
					}
					g.writeln(') {')
					ends_with_return := g.stmts_with_tmp_var(range_branch.stmts, tmp_var)
					if !ends_with_return {
						g.writeln('\tbreak;')
					}
					g.writeln('}')
				}
				g.indent--
			}
		} else {
			if branch.exprs.any(it is ast.RangeExpr) {
				range_branches << branch
				continue
			}
			for expr in branch.exprs {
				if expr is ast.EnumVal {
					covered_enum << expr.val
				}
				g.write('case ')
				g.expr(expr)
				if branch.stmts.len > 0 {
					g.write(': ')
				} else {
					g.writeln(': ')
				}
			}
		}
		g.indent++
		g.writeln('{')
		if is_expr && tmp_var.len > 0 && g.table.sym(node.return_type).kind == .sum_type {
			g.expected_cast_type = node.return_type
		}
		ends_with_return := g.stmts_with_tmp_var(branch.stmts, tmp_var)
		g.expected_cast_type = 0
		if !ends_with_return {
			g.writeln('\tbreak;')
		}
		g.indent--
		g.writeln('}')
	}
	if range_branches.len > 0 && !default_generated {
		g.write('default: ')
		g.indent++
		for range_branch in range_branches {
			g.write('if (')
			for i, expr in range_branch.exprs {
				if i > 0 {
					g.write(' || ')
				}
				if expr is ast.RangeExpr {
					// if type is unsigned and low is 0, check is unneeded
					mut skip_low := false
					if expr.low is ast.IntegerLiteral {
						if node_cond_type_unsigned && expr.low.val == '0' {
							skip_low = true
						}
					}
					g.write('(')
					if !skip_low {
						g.write('${cond_var} >= ')
						g.expr(expr.low)
						g.write(' && ')
					}
					g.write('${cond_var} <= ')
					g.expr(expr.high)
					g.write(')')
				} else {
					g.write('${cond_var} == (')
					g.expr(expr)
					g.write(')')
				}
			}
			g.writeln(') {')
			ends_with_return := g.stmts_with_tmp_var(range_branch.stmts, tmp_var)
			if !ends_with_return {
				g.writeln('\tbreak;')
			}
			g.writeln('}')
		}
		g.indent--
	}
	g.indent--
	g.writeln('}')
}

fn (mut g Gen) match_expr_classic(node ast.MatchExpr, is_expr bool, cond_var string, tmp_var string) {
	node_cond_type_unsigned := node.cond_type in [ast.u16_type, ast.u32_type, ast.u64_type]
	type_sym := g.table.sym(node.cond_type)
	use_ternary := is_expr && tmp_var.len == 0
	for j, branch in node.branches {
		is_last := j == node.branches.len - 1
		if branch.is_else || (use_ternary && is_last) {
			if node.branches.len > 1 {
				if use_ternary {
					// TODO too many branches. maybe separate ?: matches
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.writeln('else {')
				}
			}
		} else {
			if j > 0 {
				if use_ternary {
					g.write(' : ')
				} else {
					g.writeln('')
					g.write_v_source_line_info(branch.pos)
					g.write('else ')
				}
			}
			if use_ternary {
				g.write('(')
			} else {
				if j == 0 {
					g.writeln('')
				}
				g.write_v_source_line_info(branch.pos)
				g.write('if (')
			}
			for i, expr in branch.exprs {
				if i > 0 {
					g.write(' || ')
				}
				match type_sym.kind {
					.array {
						ptr_typ := g.equality_fn(node.cond_type)
						g.write('${ptr_typ}_arr_eq(${cond_var}, ')
						g.expr(expr)
						g.write(')')
					}
					.array_fixed {
						ptr_typ := g.equality_fn(node.cond_type)
						g.write('${ptr_typ}_arr_eq(${cond_var}, ')
						g.expr(expr)
						g.write(')')
					}
					.map {
						ptr_typ := g.equality_fn(node.cond_type)
						g.write('${ptr_typ}_map_eq(${cond_var}, ')
						g.expr(expr)
						g.write(')')
					}
					.string {
						g.write('string__eq(${cond_var}, ')
						g.expr(expr)
						g.write(')')
					}
					.struct_ {
						ptr_typ := g.equality_fn(node.cond_type)
						g.write('${ptr_typ}_struct_eq(${cond_var}, ')
						g.expr(expr)
						g.write(')')
					}
					else {
						if expr is ast.RangeExpr {
							// if type is unsigned and low is 0, check is unneeded
							mut skip_low := false
							if expr.low is ast.IntegerLiteral {
								if node_cond_type_unsigned && expr.low.val == '0' {
									skip_low = true
								}
							}
							g.write('(')
							if !skip_low {
								g.write('${cond_var} >= ')
								g.expr(expr.low)
								g.write(' && ')
							}
							g.write('${cond_var} <= ')
							g.expr(expr.high)
							g.write(')')
						} else {
							g.write('${cond_var} == (')
							g.expr(expr)
							g.write(')')
						}
					}
				}
			}
			if use_ternary {
				g.write(')? ')
			} else {
				g.writeln(') {')
			}
		}
		if is_expr && tmp_var.len > 0 && g.table.sym(node.return_type).kind == .sum_type {
			g.expected_cast_type = node.return_type
		}
		g.stmts_with_tmp_var(branch.stmts, tmp_var)
		g.expected_cast_type = 0
		if g.inside_ternary == 0 && node.branches.len >= 1 {
			g.write('}')
		}
	}
}
