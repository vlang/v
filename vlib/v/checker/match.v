module checker

import v.ast
import v.util
import v.token
import strings

fn (mut c Checker) match_expr(mut node ast.MatchExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	if mut node.cond is ast.ParExpr && !c.pref.translated && !c.file.is_translated {
		c.warn('unnecessary `()` in `match` condition, use `match expr {` instead of `match (expr) {`.',
			node.cond.pos)
	}
	if node.is_expr {
		c.expected_expr_type = c.expected_type
		defer {
			c.expected_expr_type = ast.void_type
		}
	}
	mut cond_type := ast.void_type
	if node.is_comptime {
		if node.cond is ast.AtExpr {
			cond_type = c.expr(mut node.cond)
		} else {
			// for field.name and generic type `T`
			if node.cond is ast.SelectorExpr {
				c.expr(mut node.cond)
			}
			cond_type = c.get_expr_type(node.cond)
		}
	} else {
		cond_type = c.expr(mut node.cond)
	}
	// we setting this here rather than at the end of the method
	// since it is used in c.match_exprs() it saves checking twice
	node.cond_type = ast.mktyp(cond_type)
	if (node.cond is ast.Ident && node.cond.is_mut)
		|| (node.cond is ast.SelectorExpr && node.cond.is_mut) {
		if node.is_comptime {
			c.error('`\$match` condition `${node.cond}` can not be mutable', node.cond.pos())
		} else {
			c.fail_if_immutable(mut node.cond)
		}
	}
	if !c.ensure_type_exists(node.cond_type, node.pos) {
		return ast.void_type
	}
	c.check_expr_option_or_result_call(node.cond, cond_type)
	cond_type_sym := c.table.sym(cond_type)
	cond_is_option := cond_type.has_flag(.option)
	node.is_sum_type = cond_type_sym.kind in [.interface, .sum_type]
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut first_iteration := true
	mut infer_cast_type := ast.void_type
	mut need_explicit_cast := false
	mut ret_type := ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut must_be_option := false
	comptime_branch_context_str := if node.is_comptime { c.gen_branch_context_string() } else { '' }
	mut comptime_match_branch_result := false
	mut comptime_match_found_branch := false
	mut comptime_match_cond_value := ''
	if node.is_comptime {
		if node.cond in [ast.ComptimeType, ast.TypeNode]
			|| (node.cond is ast.Ident && (c.is_generic_ident(node.cond.name))) {
			// must be a type `$match`
			c.inside_x_matches_type = true
		} else {
			// a value `$match`, eval the `node.cond` first
			c.expr(mut node.cond)
			if !c.type_resolver.is_generic_param_var(node.cond) {
				match mut node.cond {
					ast.StringLiteral, ast.AtExpr {
						comptime_match_cond_value = node.cond.val
					}
					ast.IntegerLiteral {
						comptime_match_cond_value = node.cond.val.str()
					}
					ast.BoolLiteral {
						comptime_match_cond_value = node.cond.val.str()
					}
					ast.Ident {
						mut cond_expr := c.find_definition(node.cond) or {
							c.error(err.msg(), node.cond.pos)
							return ast.void_type
						}
						match mut cond_expr {
							ast.StringLiteral {
								comptime_match_cond_value = cond_expr.val
							}
							ast.IntegerLiteral {
								comptime_match_cond_value = cond_expr.val.str()
							}
							ast.BoolLiteral {
								comptime_match_cond_value = cond_expr.val.str()
							}
							else {
								c.error('`${node.cond}` is not a string/int/bool literal.',
									node.cond.pos)
								return ast.void_type
							}
						}
					}
					ast.SelectorExpr {
						if c.comptime.inside_comptime_for && node.cond.field_name in ['name', 'typ'] {
							// hack: `typ` is just for bypass the error test, because we don't know it is a type match or a value match righ now
							comptime_match_cond_value = c.comptime.comptime_for_field_value.name
						} else if mut node.cond.expr is ast.Ident
							&& node.cond.gkind_field in [.typ, .unaliased_typ] {
							left_type := c.get_expr_type(node.cond.expr)
							comptime_match_cond_value = c.table.type_to_str(left_type)
						} else {
							c.error('`${node.cond}` is not `\$for` field.name.', node.cond.pos)
							return ast.void_type
						}
					}
					else {
						c.error('`\$match` cond only support string/int/bool/ident.',
							node.cond.pos())
						return ast.void_type
					}
				}
			}
		}
	}
	for mut branch in node.branches {
		if node.is_comptime {
			// `idx_str` is composed of two parts:
			// The first part represents the current context of the branch statement, `comptime_branch_context_str`, formatted like `T=int,X=string,method.name=json`
			// The second part is the branch's id.
			// This format must match what is in `cgen`.
			if branch.id == 0 {
				// this is a new branch, alloc a new id for it
				c.cur_ct_id++
				branch.id = c.cur_ct_id
			}
			idx_str := comptime_branch_context_str + '|id=${branch.id}|'
			mut c_str := ''
			if !branch.is_else {
				if c.inside_x_matches_type {
					// type $match
					for expr in branch.exprs {
						if mut node.cond is ast.ComptimeType {
							// $match $int { a {}
							branch_type := c.get_expr_type(expr)
							comptime_match_branch_result = c.type_resolver.is_comptime_type(branch_type,
								node.cond)
							c_str = '${expr} == ${c.table.type_to_str(branch_type)}'
						} else {
							is_function := c.table.final_sym(node.cond_type).kind == .function
							if !is_function {
								// $match a { $int {}
								comptime_match_branch_result = c.check_compatible_types(node.cond_type,
									'${node.cond}', expr)
								c_str = '${c.table.type_to_str(node.cond_type)} == ${expr}'
							} else {
								// $match T { FnType {} }
								branch_type := c.get_expr_type(expr)
								comptime_match_branch_result = c.table.type_to_str(node.cond_type) == c.table.type_to_str(branch_type)
								c_str = '${comptime_match_branch_result} == true'
							}
						}
						if comptime_match_branch_result {
							break
						}
					}
				} else {
					// value $match
					for mut expr in branch.exprs {
						match mut expr {
							ast.Ident {
								mut branch_expr := c.find_definition(expr) or {
									c.error(err.msg(), expr.pos)
									return ast.void_type
								}
								match mut branch_expr {
									ast.StringLiteral {
										comptime_match_branch_result = branch_expr.val == comptime_match_cond_value
									}
									ast.IntegerLiteral {
										comptime_match_branch_result = branch_expr.val.str() == comptime_match_cond_value
									}
									ast.BoolLiteral {
										comptime_match_branch_result = branch_expr.val.str() == comptime_match_cond_value
									}
									else {
										c.error('`${expr}` is not a string/int/bool literal.',
											expr.pos)
										return ast.void_type
									}
								}
								c_str = '${node.cond} == ${expr.name}'
							}
							ast.SelectorExpr {}
							ast.StringLiteral {
								c_str = '${node.cond} == ${expr}'
								comptime_match_branch_result = comptime_match_cond_value == expr.val
							}
							ast.IntegerLiteral {
								c_str = '${node.cond} == ${expr.val}'
								comptime_match_branch_result = comptime_match_cond_value == expr.val.str()
							}
							ast.BoolLiteral {
								c_str = '${node.cond} == ${expr.val}'
								comptime_match_branch_result = comptime_match_cond_value == expr.val.str()
							}
							else {
								c.error('`\$match` branch only support string/int/bool types',
									node.cond.pos())
								return ast.void_type
							}
						}
						if comptime_match_branch_result {
							break
						}
					}
				}
				if comptime_match_branch_result {
					comptime_match_found_branch = true
				}
				// set `comptime_is_true` which can be used by `cgen`
				c.table.comptime_is_true[idx_str] = ast.ComptTimeCondResult{
					val:   comptime_match_branch_result
					c_str: c_str
				}
			} else {
				comptime_match_branch_result = !comptime_match_found_branch
				c.table.comptime_is_true[idx_str] = ast.ComptTimeCondResult{
					val:   comptime_match_branch_result
					c_str: ''
				}
			}
		}

		if !node.is_comptime || (node.is_comptime && comptime_match_branch_result) {
			if node.is_expr {
				c.stmts_ending_with_expression(mut branch.stmts, c.expected_or_type)
			} else {
				c.stmts(mut branch.stmts)
			}
		}
		c.smartcast_mut_pos = token.Pos{}
		c.smartcast_cond_pos = token.Pos{}
		if node.is_expr {
			if branch.stmts.len == 0 && ret_type != ast.void_type {
				c.error('`match` expression requires an expression as the last statement of every branch',
					branch.branch_pos)
			}
		}
		if !branch.is_else && cond_is_option && branch.exprs.any(it !is ast.None) {
			c.error('`match` expression with Option type only checks against `none`, to match its value you must unwrap it first `var?`',
				branch.pos)
		}
		if cond_type_sym.kind == .none {
			c.error('`none` cannot be a match condition', node.pos)
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 && node.is_expr {
			mut stmt := branch.stmts.last()
			if mut stmt is ast.ExprStmt {
				c.expected_type = if c.expected_expr_type != ast.void_type {
					c.expected_expr_type
				} else {
					node.expected_type
				}
				branch.is_comptime_err = stmt.expr is ast.ComptimeCall
					&& stmt.expr.kind in [.compile_error, .compile_warn]
				expr_type := if branch.is_comptime_err {
					c.expected_type
				} else {
					c.unwrap_generic(if stmt.expr is ast.CallExpr {
						stmt.typ
					} else {
						c.expr(mut stmt.expr)
					})
				}
				unwrapped_expected_type := c.unwrap_generic(node.expected_type)
				must_be_option = must_be_option || expr_type == ast.none_type
				stmt.typ = expr_type
				if first_iteration {
					if unwrapped_expected_type.has_option_or_result()
						|| c.table.type_kind(unwrapped_expected_type) in [.sum_type, .multi_return] {
						c.check_match_branch_last_stmt(stmt, unwrapped_expected_type,
							expr_type)
						ret_type = node.expected_type
					} else {
						ret_type = expr_type
						if expr_type.is_ptr() {
							if stmt.expr is ast.Ident && stmt.expr.obj is ast.Var
								&& c.table.is_interface_var(stmt.expr.obj) {
								ret_type = expr_type.deref()
							} else if mut stmt.expr is ast.PrefixExpr
								&& stmt.expr.right is ast.Ident {
								ident := stmt.expr.right as ast.Ident
								if ident.obj is ast.Var && c.table.is_interface_var(ident.obj) {
									ret_type = expr_type.deref()
								}
							}
						}
						c.expected_expr_type = expr_type
					}
					infer_cast_type = stmt.typ
					if mut stmt.expr is ast.CastExpr {
						need_explicit_cast = true
						infer_cast_type = stmt.expr.typ
					}
				} else {
					if ret_type.idx() != expr_type.idx() {
						if unwrapped_expected_type.has_option_or_result()
							&& c.table.sym(stmt.typ).kind == .struct
							&& !c.check_types(expr_type, c.unwrap_generic(ret_type))
							&& c.type_implements(stmt.typ, ast.error_type, node.pos) {
							stmt.expr = ast.CastExpr{
								expr:      stmt.expr
								typname:   'IError'
								typ:       ast.error_type
								expr_type: stmt.typ
								pos:       node.pos
							}
							stmt.typ = ast.error_type
						} else {
							c.check_match_branch_last_stmt(stmt, c.unwrap_generic(ret_type),
								expr_type)
							if ret_type.is_number() && expr_type.is_number() && !c.inside_return {
								ret_type = c.promote_num(ret_type, expr_type)
							}
						}
					}
					if must_be_option && ret_type == ast.none_type && expr_type != ret_type {
						ret_type = expr_type.set_flag(.option)
					}
					if stmt.typ != ast.error_type && !is_noreturn_callexpr(stmt.expr) {
						ret_sym := c.table.sym(ret_type)
						stmt_sym := c.table.sym(stmt.typ)
						if ret_sym.kind !in [.sum_type, .interface]
							&& stmt_sym.kind in [.sum_type, .interface] {
							c.error('return type mismatch, it should be `${ret_sym.name}`, but it is instead `${c.table.type_to_str(expr_type)}`',
								stmt.pos)
						}
						if ret_type.nr_muls() != stmt.typ.nr_muls()
							&& stmt.typ.idx() !in [ast.voidptr_type_idx, ast.nil_type_idx] {
							type_name := '&'.repeat(ret_type.nr_muls()) + ret_sym.name
							c.error('return type mismatch, it should be `${type_name}`, but it is instead `${c.table.type_to_str(expr_type)}`',
								stmt.pos)
						}
					}
					if !node.is_sum_type {
						if mut stmt.expr is ast.CastExpr {
							expr_typ_sym := c.table.sym(stmt.expr.typ)
							if need_explicit_cast {
								if infer_cast_type != stmt.expr.typ
									&& expr_typ_sym.kind !in [.interface, .sum_type] {
									c.error('the type of the last expression in the first match branch was an explicit `${c.table.type_to_str(infer_cast_type)}`, not `${c.table.type_to_str(stmt.expr.typ)}`',
										stmt.pos)
								}
							} else {
								if infer_cast_type != stmt.expr.typ
									&& expr_typ_sym.kind !in [.interface, .sum_type]
									&& c.promote_num(stmt.expr.typ, ast.int_type) != ast.int_type {
									c.error('the type of the last expression of the first match branch was `${c.table.type_to_str(infer_cast_type)}`, which is not compatible with `${c.table.type_to_str(stmt.expr.typ)}`',
										stmt.pos)
								}
							}
						} else {
							if mut stmt.expr is ast.IntegerLiteral {
								cast_type_sym := c.table.sym(infer_cast_type)
								num := stmt.expr.val.i64()
								mut needs_explicit_cast := false

								match cast_type_sym.kind {
									.u8 {
										if !(num >= min_u8 && num <= max_u8) {
											needs_explicit_cast = true
										}
									}
									.u16 {
										if !(num >= min_u16 && num <= max_u16) {
											needs_explicit_cast = true
										}
									}
									.u32 {
										if !(num >= min_u32 && num <= max_u32) {
											needs_explicit_cast = true
										}
									}
									.u64 {
										if !(num >= min_u64 && num <= max_u64) {
											needs_explicit_cast = true
										}
									}
									.i8 {
										if !(num >= min_i32 && num <= max_i32) {
											needs_explicit_cast = true
										}
									}
									.i16 {
										if !(num >= min_i16 && num <= max_i16) {
											needs_explicit_cast = true
										}
									}
									.i32 {
										if !(num >= min_i32 && num <= max_i32) {
											needs_explicit_cast = true
										}
									}
									.int {
										$if new_int ? && x64 {
											if !(num >= min_i64 && num <= max_i64) {
												needs_explicit_cast = true
											}
										} $else {
											if !(num >= min_i32 && num <= max_i32) {
												needs_explicit_cast = true
											}
										}
									}
									.i64 {
										if !(num >= min_i64 && num <= max_i64) {
											needs_explicit_cast = true
										}
									}
									.int_literal {
										needs_explicit_cast = false
									}
									else {}
								}
								if needs_explicit_cast {
									c.error('${num} does not fit the range of `${c.table.type_to_str(infer_cast_type)}`',
										stmt.pos)
								}
							}
						}
					}
				}
			} else if stmt !in [ast.Return, ast.BranchStmt] {
				if ret_type != ast.void_type {
					c.error('`match` expression requires an expression as the last statement of every branch',
						stmt.pos)
				}
			} else if c.inside_return && mut stmt is ast.Return && ret_type == ast.void_type {
				ret_type = if stmt.types.len > 0 { stmt.types[0] } else { c.expected_type }
			}
		}
		first_iteration = false
		if has_return := c.has_return(branch.stmts) {
			if has_return {
				nbranches_with_return++
			} else {
				nbranches_without_return++
			}
		}
	}
	if nbranches_with_return > 0 {
		if nbranches_with_return == node.branches.len {
			// an exhaustive match, and all branches returned
			c.returns = true
		}
		if nbranches_without_return > 0 {
			// some of the branches did not return
			c.returns = false
		}
	}
	if ret_type == ast.none_type {
		c.error('invalid match expression, must supply at least one value other than `none`',
			node.pos)
	}
	node.return_type = if must_be_option { ret_type.set_flag(.option) } else { ret_type }
	cond_var := c.get_base_name(&node.cond)
	if cond_var != '' {
		mut cond_is_auto_heap := false
		for branch in node.branches {
			if v := branch.scope.find_var(cond_var) {
				if v.is_auto_heap {
					cond_is_auto_heap = true
					break
				}
			}
		}
		if cond_is_auto_heap {
			for branch in node.branches {
				mut v := branch.scope.find_var(cond_var) or { continue }
				v.is_auto_heap = true
			}
		}
	}
	return node.return_type
}

fn (mut c Checker) check_match_branch_last_stmt(last_stmt ast.ExprStmt, ret_type ast.Type, expr_type ast.Type) {
	if !c.check_types(ret_type, expr_type) && !c.check_types(expr_type, ret_type) {
		ret_sym := c.table.sym(ret_type)
		is_noreturn := is_noreturn_callexpr(last_stmt.expr)
		if !(ret_sym.kind == .sum_type && (ret_type.has_flag(.generic)
			|| c.table.is_sumtype_or_in_variant(ret_type, expr_type))) && !is_noreturn {
			expr_sym := c.table.sym(expr_type)
			if expr_sym.kind == .multi_return && ret_sym.kind == .multi_return {
				ret_types := ret_sym.mr_info().types
				expr_types := expr_sym.mr_info().types.map(ast.mktyp(it))
				if expr_types == ret_types {
					return
				}
			}
			if expr_type != ast.none_type && ret_type != ast.none_type {
				c.error('return type mismatch, it should be `${ret_sym.name}`, but it is instead `${c.table.type_to_str(expr_type)}`',
					last_stmt.pos)
			}
		}
	} else if expr_type == ast.void_type && ret_type.idx() == ast.void_type_idx
		&& ret_type.has_option_or_result() {
		c.error('`${last_stmt.expr}` used as value', last_stmt.pos)
	}
}

fn (mut c Checker) get_comptime_number_value(mut expr ast.Expr) ?i64 {
	if mut expr is ast.CharLiteral {
		return expr.val[0]
	}
	if mut expr is ast.IntegerLiteral {
		return expr.val.i64()
	}
	if mut expr is ast.CastExpr && expr.expr is ast.IntegerLiteral {
		return expr.expr.val.i64()
	}
	if mut expr is ast.Ident {
		if mut obj := c.table.global_scope.find_const(expr.full_name()) {
			if obj.typ == 0 {
				obj.typ = c.expr(mut obj.expr)
			}
			return c.get_comptime_number_value(mut obj.expr)
		}
	}
	return none
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, cond_type_sym ast.TypeSymbol) {
	c.expected_type = node.expected_type
	cond_sym := c.table.sym(node.cond_type)
	mut enum_ref_checked := false
	mut is_comptime_value_match := false
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	for branch_i, _ in node.branches {
		mut branch := node.branches[branch_i]
		mut expr_types := []ast.TypeNode{}
		for k, mut expr in branch.exprs {
			mut key := ''
			// TODO: investigate why enums are different here:
			if expr !is ast.EnumVal && !(node.is_comptime && expr is ast.ComptimeType) {
				// ensure that the sub expressions of the branch are actually checked, before anything else:
				_ := c.expr(mut expr)
			}
			if expr is ast.TypeNode && cond_sym.kind == .struct {
				c.error('struct instances cannot be matched by type name, they can only be matched to other instances of the same struct type',
					branch.pos)
			}
			mut is_comptime := false
			if c.comptime.inside_comptime_for {
				// it is a compile-time field.typ checking
				if mut node.cond is ast.SelectorExpr {
					is_comptime = node.cond.expr_type == c.field_data_type
						&& node.cond.field_name == 'typ'
				}
			}
			if mut expr is ast.TypeNode && cond_sym.is_primitive() && !is_comptime
				&& !node.is_comptime {
				c.error('matching by type can only be done for sum types, generics, interfaces, `${node.cond}` is none of those',
					branch.pos)
			}
			if mut expr is ast.RangeExpr {
				// Allow for `match enum_value { 4..5 { } }`, even though usually int and enum values,
				// are considered incompatible outside unsafe{}, and are not allowed to be compared directly
				if cond_sym.kind != .enum && !c.check_types(expr.typ, node.cond_type) {
					mcstype := c.table.type_to_str(node.cond_type)
					brstype := c.table.type_to_str(expr.typ)
					c.add_error_detail('')
					c.add_error_detail('match condition type: ${mcstype}')
					c.add_error_detail('          range type: ${brstype}')
					c.error('the range type and the match condition type should match',
						expr.pos)
				}
				mut low_value_higher_than_high_value := false
				mut low := i64(0)
				mut high := i64(0)
				mut both_low_and_high_are_known := false
				if low_value := c.get_comptime_number_value(mut expr.low) {
					low = low_value
					if high_value := c.get_comptime_number_value(mut expr.high) {
						high = high_value
						both_low_and_high_are_known = true
						if low_value > high_value {
							low_value_higher_than_high_value = true
						}
					} else {
						if expr.high !is ast.EnumVal {
							c.error('match branch range expressions need the end value to be known at compile time (only enums, const or literals are supported)',
								expr.high.pos())
						}
					}
				} else {
					if expr.low !is ast.EnumVal {
						c.error('match branch range expressions need the start value to be known at compile time (only enums, const or literals are supported)',
							expr.low.pos())
					}
				}
				if low_value_higher_than_high_value {
					c.error('the start value `${low}` should be lower than the end value `${high}`',
						branch.pos)
				}
				if both_low_and_high_are_known {
					high_low_cutoff := 1000
					if high - low > high_low_cutoff {
						c.note('more than ${high_low_cutoff} possibilities (${low} ... ${high}) in match range may affect compile time',
							branch.pos)
					}
					for i in low .. high + 1 {
						key = i.str()
						val := if key in branch_exprs { branch_exprs[key] } else { 0 }
						if val == 1 {
							c.error('match case `${key}` is handled more than once', branch.pos)
						}
						branch_exprs[key] = val + 1
					}
				}
				continue
			}
			is_type_node := expr is ast.TypeNode || expr is ast.ComptimeType
			match mut expr {
				ast.TypeNode {
					key = c.table.type_to_str(expr.typ)
					expr_types << expr
				}
				ast.EnumVal {
					key = expr.val
					if !enum_ref_checked {
						enum_ref_checked = true
						if node.cond_type.is_ptr() {
							c.error('missing `*` dereferencing `${node.cond}` in match statement',
								node.cond.pos())
						}
					}
				}
				else {
					key = (*expr).str()
				}
			}
			val := if key in branch_exprs { branch_exprs[key] } else { 0 }
			if val == 1 {
				c.error('match case `${key}` is handled more than once', branch.pos)
			}
			c.expected_type = node.cond_type
			if is_type_node {
				c.inside_x_matches_type = true
			} else {
				if node.is_comptime {
					is_comptime_value_match = true
				}
			}
			if node.is_comptime {
				if is_type_node {
					if is_comptime_value_match {
						// type branch in a value match
						c.error('can not matching a type in a value `\$match`', expr.pos())
						return
					}
				} else if c.inside_x_matches_type {
					// value branch in a type match
					if expr in [ast.IntegerLiteral, ast.BoolLiteral, ast.StringLiteral] {
						c.error('can not matching a value in a type `\$match`', expr.pos())
						return
					}
				}
				if !is_type_node {
					// value check should match cond's type
					if expr is ast.IntegerLiteral {
						if mut node.cond is ast.ComptimeType {
							if node.cond.kind != .int {
								c.error('can not matching a int value(`${expr}`) in a non int type `\$match`, `${node.cond}` type is `${node.cond.kind}`',
									expr.pos())
								return
							}
						} else if node.cond_type !in ast.integer_type_idxs {
							c.error('can not matching a int value(`${expr}`) in a non int type `\$match`, `${node.cond}` type is `${c.table.type_to_str(node.cond_type)}`',
								expr.pos())
							return
						}
					} else if expr is ast.BoolLiteral && node.cond_type != ast.bool_type {
						c.error('can not matching a bool value(`${expr}`) in a non bool type `\$match`, `${node.cond}` type is `${c.table.type_to_str(node.cond_type)}`',
							expr.pos())
						return
					} else if expr is ast.StringLiteral {
						if mut node.cond is ast.ComptimeType {
							if node.cond.kind != .string {
								c.error('can not matching a string value(`${expr}`) in a non string type `\$match`, `${node.cond}` type is `${node.cond.kind}`',
									expr.pos())
								return
							}
						} else if node.cond_type != ast.string_type {
							c.error('can not matching a string value(`${expr}`) in a non string type `\$match`, `${node.cond}` type is `${c.table.type_to_str(node.cond_type)}`',
								expr.pos())
							return
						}
					}
				}
			} else {
				expr_type := c.expr(mut expr)
				if expr_type.idx() == 0 {
					// parser failed, stop checking
					return
				}
				expr_type_sym := c.table.sym(expr_type)
				if cond_type_sym.kind == .interface {
					// TODO
					// This generates a memory issue with TCC
					// Needs to be checked later when TCC errors are fixed
					// Current solution is to move expr.pos() to its own statement
					// c.type_implements(expr_type, c.expected_type, expr.pos())
					expr_pos := expr.pos()
					if c.type_implements(expr_type, c.expected_type, expr_pos) {
						if !expr_type.is_any_kind_of_pointer() && !c.inside_unsafe {
							if expr_type_sym.kind != .interface {
								c.mark_as_referenced(mut &branch.exprs[k], true)
							}
						}
					}
				} else if cond_type_sym.info is ast.SumType {
					if expr_type !in cond_type_sym.info.variants {
						expr_str := c.table.type_to_str(expr_type)
						expect_str := c.table.type_to_str(node.cond_type)
						sumtype_variant_names := cond_type_sym.info.variants.map(c.table.type_to_str_using_aliases(it,
							{}))
						suggestion := util.new_suggestion(expr_str, sumtype_variant_names)
						c.error(suggestion.say('`${expect_str}` has no variant `${expr_str}`'),
							expr.pos())
					}
				} else if cond_type_sym.info is ast.Alias && expr_type_sym.info is ast.Struct {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					c.error('cannot match alias type `${expect_str}` with `${expr_str}`',
						expr.pos())
				} else if !c.check_types(expr_type, node.cond_type) && !is_comptime {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					c.error('cannot match `${expect_str}` with `${expr_str}`', expr.pos())
				}
			}
			branch_exprs[key] = val + 1
		}
		// when match is type matching, then register smart cast for every branch
		if expr_types.len > 0 {
			if cond_type_sym.kind in [.sum_type, .interface] {
				mut expr_type := ast.no_type
				if expr_types.len > 1 {
					mut agg_name := strings.new_builder(20)
					mut agg_cname := strings.new_builder(20)
					agg_name.write_string('(')
					for i, expr in expr_types {
						if i > 0 {
							agg_name.write_string(' | ')
							agg_cname.write_string('___')
						}
						type_str := c.table.type_to_str(expr.typ)
						name := if c.is_builtin_mod { type_str } else { '${c.mod}.${type_str}' }
						agg_name.write_string(name)
						agg_cname.write_string(util.no_dots(name))
					}
					agg_name.write_string(')')
					name := agg_name.str()
					existing_idx := c.table.type_idxs[name]
					if existing_idx > 0 {
						expr_type = existing_idx
					} else {
						expr_type = c.table.register_sym(ast.TypeSymbol{
							name:  name
							cname: agg_cname.str()
							kind:  .aggregate
							mod:   c.mod
							info:  ast.Aggregate{
								sum_type: node.cond_type
								types:    expr_types.map(it.typ)
							}
						})
					}
				} else {
					expr_type = expr_types[0].typ
				}

				c.smartcast(mut node.cond, node.cond_type, expr_type, mut branch.scope,
					false, false)
			}
		}
	}
	// check that expressions are exhaustive
	// this is achieved either by putting an else
	// or, when the match is on a sum type or an enum
	// by listing all variants or values
	mut is_exhaustive := true
	mut unhandled := []string{}
	if node.cond_type == ast.bool_type {
		variants := ['true', 'false']
		for v in variants {
			if v !in branch_exprs {
				is_exhaustive = false
				unhandled << '`${v}`'
			}
		}
	} else {
		match cond_type_sym.info {
			ast.SumType {
				for v in cond_type_sym.info.variants {
					v_str := c.table.type_to_str(v)
					if v_str !in branch_exprs {
						is_exhaustive = false
						unhandled << '`${v_str}`'
					}
				}
			}
			//
			ast.Enum {
				for v in cond_type_sym.info.vals {
					if v !in branch_exprs {
						is_exhaustive = false
						unhandled << '`.${v}`'
					}
				}
				if cond_type_sym.info.is_flag {
					is_exhaustive = false
				}
			}
			else {
				is_exhaustive = false
			}
		}
	}
	if node.branches.len == 0 {
		c.error('`match` must have at least two branches including `else`, or an exhaustive set of branches',
			node.pos)
		return
	}
	mut else_branch := node.branches.last()
	mut has_else, mut has_non_else := else_branch.is_else, !else_branch.is_else
	for branch in node.branches[..node.branches.len - 1] {
		if branch.is_else {
			if has_else {
				c.error('`match` can have only one `else` branch', branch.pos)
			}
			c.error('`else` must be the last branch of `match`', branch.pos)
			else_branch = branch
			has_else = true
		} else {
			has_non_else = true
		}
	}
	if !has_non_else {
		c.error('`match` must have at least one non `else` branch', else_branch.pos)
	}
	if is_exhaustive {
		if has_else && !c.pref.translated && !c.file.is_translated {
			c.error('match expression is exhaustive, `else` is unnecessary', else_branch.pos)
		}
		return
	}
	if has_else || node.is_comptime {
		return
	}
	mut err_details := 'match must be exhaustive'
	if unhandled.len > 0 {
		err_details += ' (add match branches for: '
		if unhandled.len < c.match_exhaustive_cutoff_limit {
			err_details += unhandled.join(', ')
		} else {
			remaining := unhandled.len - c.match_exhaustive_cutoff_limit
			err_details += unhandled[0..c.match_exhaustive_cutoff_limit].join(', ')
			if remaining > 0 {
				err_details += ', and ${remaining} others ...'
			}
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}
