// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn for_in_val_type(base_type ast.Type, is_mut bool, is_ref bool) ast.Type {
	if base_type == 0 {
		return base_type
	}
	if is_mut || is_ref {
		if base_type.has_flag(.option) {
			return base_type.set_flag(.option_mut_param_t)
		}
		if !base_type.is_any_kind_of_pointer() {
			return base_type.ref()
		}
	}
	return base_type
}

// A labeled continue jumps back to this gate instead of forward over later
// declarations in the loop body, which avoids gcc -Wjump-misses-init.
fn (mut g Gen) write_labeled_continue_gate(label string, prefix string) {
	if label.len == 0 {
		return
	}
	continue_flag := labeled_continue_flag_name(label)
	continue_entry_label := labeled_continue_entry_label_name(label)
	g.writeln('${prefix}bool ${continue_flag} = false;')
	g.writeln('${prefix}${continue_entry_label}: {}')
	g.writeln('${prefix}if (${continue_flag}) goto ${label}__continue;')
}

fn (mut g Gen) for_c_stmt(node ast.ForCStmt) {
	g.loop_depth++
	if node.is_multi {
		g.is_vlines_enabled = false
		g.inside_for_c_stmt = true
		if node.label.len > 0 {
			g.writeln('${node.label}:')
		}
		g.writeln('{')
		g.indent++
		if node.has_init {
			g.stmt(node.init)
			if node.init is ast.ExprStmt {
				g.write('; ')
			}
		}
		g.writeln('bool _is_first = true;')
		g.writeln('while (true) {')
		g.writeln('\tif (_is_first) {')
		g.writeln('\t\t_is_first = false;')
		g.writeln('\t} else {')
		if node.has_inc {
			g.indent++
			g.stmt(node.inc)
			g.writeln(';')
			g.indent--
		}
		g.writeln('}')
		if node.has_cond {
			g.write('if (!(')
			g.expr(node.cond)
			g.writeln(')) break;')
		}
		g.is_vlines_enabled = true
		g.inside_for_c_stmt = false
		g.write_labeled_continue_gate(node.label, '')
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.writeln('}')
		g.indent--
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	} else {
		g.is_vlines_enabled = false
		g.inside_for_c_stmt = true
		if node.label.len > 0 {
			g.writeln('${node.label}:')
		}
		g.set_current_pos_as_last_stmt_pos()
		g.skip_stmt_pos = true
		g.write('for (')
		if !node.has_init {
			g.write('; ')
		} else {
			g.stmt(node.init)
			if node.init is ast.ExprStmt {
				g.write('; ')
			}
			// Remove excess return and add space
			if g.out.last_n(1) == '\n' {
				g.go_back(1)
				g.empty_line = false
				g.write(' ')
			}
		}
		if node.has_cond {
			g.expr(node.cond)
		}
		g.write('; ')
		if node.has_inc {
			mut processed := false
			if node.inc is ast.ExprStmt && node.inc.expr is ast.ConcatExpr {
				for inc_expr_idx, inc_expr in node.inc.expr.vals {
					g.expr(inc_expr)
					if inc_expr_idx < node.inc.expr.vals.len - 1 {
						g.write(', ')
					}
				}
				processed = true
			}
			if !processed {
				g.stmt(node.inc)
			}
		}
		g.writeln(') {')
		g.skip_stmt_pos = false
		g.is_vlines_enabled = true
		g.inside_for_c_stmt = false
		g.write_labeled_continue_gate(node.label, '')
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.write_defer_stmts(node.scope, false, node.pos)
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	}
	g.loop_depth--
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	g.loop_depth++
	g.is_vlines_enabled = false
	if node.label.len > 0 {
		g.writeln('${node.label}:')
	}
	g.writeln('for (;;) {')
	if !node.is_inf {
		g.indent++
		g.set_current_pos_as_last_stmt_pos()
		g.write('if (!(')
		g.expr(node.cond)
		g.writeln(')) break;')
		g.indent--
	}
	g.is_vlines_enabled = true
	g.write_labeled_continue_gate(node.label, '\t')
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}
	g.write_defer_stmts(node.scope, false, node.pos)
	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('${node.label}__break: {}')
	}
	g.loop_depth--
}

fn (mut g Gen) for_in_stmt(node_ ast.ForInStmt) {
	mut node := node_
	mut is_comptime := false
	mut param_key_type := ast.Type(0)
	mut param_val_type := ast.Type(0)
	mut scope_cond_type := ast.Type(0)
	mut resolved_cond_expr := node.cond
	if node.cond is ast.Ident {
		mut cond_ident := node.cond as ast.Ident
		if node.scope != unsafe { nil } {
			cond_ident.scope = node.scope
		} else if g.file.scope != unsafe { nil } {
			cond_ident.scope = g.file.scope.innermost(node.pos.pos)
		}
		if cond_ident.scope != unsafe { nil } {
			if scope_var := cond_ident.scope.find_var(cond_ident.name) {
				cond_ident.obj = *scope_var
			}
		}
		resolved_cond_expr = cond_ident
		param_cond_type := g.resolve_current_fn_generic_param_type(cond_ident.name)
		scope_cond_type = g.resolved_scope_var_type(cond_ident)
		// Don't let an aggregate/sumtype scope type override a more specific
		// cond_type (e.g., a concrete array type from the aggregate handler).
		if scope_cond_type != 0 && node.cond_type != 0 && node.cond_type != scope_cond_type {
			scope_sym := g.table.final_sym(scope_cond_type)
			if scope_sym.kind == .aggregate || scope_sym.kind == .sum_type {
				scope_cond_type = 0
			}
		}
		if scope_cond_type != 0 {
			node.cond_type = scope_cond_type
		} else if param_cond_type != 0 {
			node.cond_type = param_cond_type
		}
		param_key_type = g.resolve_current_fn_generic_param_key_type(cond_ident.name)
		param_val_type = g.resolve_current_fn_generic_param_value_type(cond_ident.name)
	}
	resolved_cond_type := g.resolved_expr_type(resolved_cond_expr, node.cond_type)
	if resolved_cond_type != 0 {
		// Don't let an aggregate/sumtype resolved type override a more specific
		// cond_type (e.g., a concrete array type from the aggregate handler).
		resolved_sym := g.table.final_sym(resolved_cond_type)
		if !(resolved_sym.kind in [.aggregate, .sum_type] && node.cond_type != 0
			&& node.cond_type != resolved_cond_type
			&& g.table.final_sym(node.cond_type).kind !in [.aggregate, .sum_type]) {
			node.cond_type = resolved_cond_type
		}
	}
	if scope_cond_type != 0 {
		node.cond_type = scope_cond_type
	}
	node.cond_type = g.recheck_concrete_type(node.cond_type)
	if scope_cond_type != 0 {
		node.cond_type = scope_cond_type
	}
	if node.cond_type != 0 {
		resolved_cond_sym := g.table.final_sym(g.unwrap_generic(node.cond_type))
		if resolved_cond_sym.kind in [.array, .array_fixed, .map, .string, .aggregate, .alias] {
			node.kind = resolved_cond_sym.kind
		}
		if node.kind in [.array, .array_fixed, .map, .string] {
			unwrapped_cond_type := g.unwrap_generic(g.recheck_concrete_type(node.cond_type))
			if node.key_var.len > 0 {
				node.key_type = if param_key_type != 0 {
					param_key_type
				} else {
					match resolved_cond_sym.kind {
						.map { resolved_cond_sym.map_info().key_type }
						else { ast.int_type }
					}
				}
				node.scope.update_var_type(node.key_var, node.key_type)
			}
			base_val_type := if scope_cond_type != 0 {
				g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(scope_cond_type)))
			} else if param_val_type != 0 {
				param_val_type
			} else {
				g.recheck_concrete_type(g.table.value_type(unwrapped_cond_type))
			}
			node.val_type = for_in_val_type(base_val_type, node.val_is_mut, node.val_is_ref)
			node.scope.update_var_type(node.val_var, node.val_type)
		}
	}

	if (node.cond is ast.Ident && node.cond.ct_expr) || node.cond is ast.ComptimeSelector {
		mut unwrapped_typ := g.unwrap_generic(g.recheck_concrete_type(node.cond_type))
		ctyp := g.type_resolver.get_type(node.cond)
		if ctyp != ast.void_type {
			unwrapped_typ = g.unwrap_generic(g.recheck_concrete_type(ctyp))
			is_comptime = true
		}

		mut unwrapped_sym := g.table.sym(unwrapped_typ)

		node.cond_type = unwrapped_typ
		base_val_type := g.recheck_concrete_type(g.table.value_type(unwrapped_typ))
		node.val_type = for_in_val_type(base_val_type, node.val_is_mut, node.val_is_ref)
		node.scope.update_var_type(node.val_var, node.val_type)
		node.kind = unwrapped_sym.kind

		if is_comptime {
			g.type_resolver.update_ct_type(node.val_var, node.val_type)
			node.scope.update_ct_var_kind(node.val_var, .value_var)

			defer(fn) {
				g.type_resolver.type_map.delete(node.val_var)
			}
		}

		if node.key_var.len > 0 {
			key_type := if param_key_type != 0 {
				param_key_type
			} else {
				match unwrapped_sym.kind {
					.map { unwrapped_sym.map_info().key_type }
					else { ast.int_type }
				}
			}
			node.key_type = key_type
			node.scope.update_var_type(node.key_var, key_type)

			if is_comptime {
				g.type_resolver.update_ct_type(node.key_var, node.key_type)
				node.scope.update_ct_var_kind(node.key_var, .key_var)

				defer(fn) {
					g.type_resolver.type_map.delete(node.key_var)
				}
			}
		}
	}

	if node.kind == .any && !is_comptime {
		mut unwrapped_typ := if scope_cond_type != 0 {
			g.unwrap_generic(scope_cond_type)
		} else {
			g.unwrap_generic(g.recheck_concrete_type(node.cond_type))
		}
		mut unwrapped_sym := g.table.sym(unwrapped_typ)
		node.kind = unwrapped_sym.kind
		node.cond_type = unwrapped_typ
		if node.key_var.len > 0 {
			key_type := if param_key_type != 0 {
				param_key_type
			} else {
				match unwrapped_sym.kind {
					.map { unwrapped_sym.map_info().key_type }
					else { ast.int_type }
				}
			}
			node.key_type = key_type
			node.scope.update_var_type(node.key_var, key_type)
		}
		base_val_type := g.recheck_concrete_type(g.table.value_type(unwrapped_typ))
		node.val_type = for_in_val_type(base_val_type, node.val_is_mut, node.val_is_ref)
		node.scope.update_var_type(node.val_var, node.val_type)
	} else if node.kind == .alias {
		mut unwrapped_typ := if scope_cond_type != 0 {
			g.unwrap_generic(scope_cond_type)
		} else {
			g.unwrap_generic(g.recheck_concrete_type(node.cond_type))
		}
		mut unwrapped_sym := g.table.final_sym(unwrapped_typ)
		node.kind = unwrapped_sym.kind
		node.cond_type = unwrapped_typ
		if node.key_var.len > 0 {
			key_type := if param_key_type != 0 {
				param_key_type
			} else {
				match unwrapped_sym.kind {
					.map { unwrapped_sym.map_info().key_type }
					else { ast.int_type }
				}
			}
			node.key_type = key_type
			node.scope.update_var_type(node.key_var, key_type)
		}
		base_val_type :=
			g.recheck_concrete_type(g.table.value_type(g.table.unaliased_type(unwrapped_typ)))
		node.val_type = for_in_val_type(base_val_type, node.val_is_mut, node.val_is_ref)
		node.scope.update_var_type(node.val_var, node.val_type)
	}
	g.loop_depth++
	if node.label.len > 0 {
		g.writeln('\t${node.label}: {}')
	}
	if node.is_range {
		// `for x in 1..10 {`
		i := if node.val_var == '_' { g.new_tmp_var() } else { c_name(node.val_var) }
		plus_plus_i := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${i}=builtin__overflow__add_i64(${i},1)'
			} $else {
				'${i}=builtin__overflow__add_i32(${i},1)'
			}
		} else {
			'++${i}'
		}
		val_typ := ast.mktyp(node.val_type)
		g.write('for (${g.styp(val_typ)} ${i} = ')
		g.expr(node.cond)
		g.write('; ${i} < ')
		g.expr(node.high)
		g.writeln('; ${plus_plus_i}) {')
	} else if node.kind == .array {
		// `for num in nums {`
		// g.writeln('// FOR IN array')
		if node.cond_type != 0 {
			// Use scope_cond_type only if it's a concrete container type.
			// Skip if it's an aggregate/sumtype (e.g., from a match arm
			// smartcast) as value_type would return void for those.
			use_scope_cond := scope_cond_type != 0
				&& g.table.final_sym(scope_cond_type).kind !in [.aggregate, .sum_type]
			resolved_val_type := if use_scope_cond {
				g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(scope_cond_type)))
			} else if param_val_type != 0 {
				param_val_type
			} else {
				g.recheck_concrete_type(g.table.value_type(g.unwrap_generic(g.recheck_concrete_type(node.cond_type))))
			}
			if resolved_val_type != 0 {
				node.val_type = for_in_val_type(resolved_val_type, node.val_is_mut, node.val_is_ref)
				node.scope.update_var_type(node.val_var, node.val_type)
			}
		}
		$if trace_ci_fixes ? {
			if g.cur_fn != unsafe { nil } && g.cur_fn.name in ['arrays.flatten', 'arrays.group_by'] {
				trace_scope_cond_type := if node.cond is ast.Ident {
					g.resolved_scope_var_type(node.cond as ast.Ident)
				} else {
					ast.no_type
				}
				eprintln('cgen for ${g.cur_fn.name} val=${node.val_var} val_type=${g.table.type_to_str(node.val_type)} cond_type=${g.table.type_to_str(node.cond_type)} scope_cond=${if trace_scope_cond_type != 0 {
					g.table.type_to_str(trace_scope_cond_type)
				} else {
					'<none>'
				}} cur=${g.cur_concrete_types.map(g.table.type_to_str(it))}')
			}
		}
		mut styp := g.styp(node.val_type)
		mut val_sym := g.table.sym(node.val_type)
		op_field := if node.cond_type.has_flag(.shared_f) {
			'->val.'
		} else if node.cond_type.is_ptr() || resolved_cond_expr.is_auto_deref_var() {
			'->'
		} else {
			g.dot_or_ptr(node.cond_type)
		}

		mut cond_var := ''
		// Check if the cond has an or-block that unwraps the option
		cond_has_or_block := (node.cond is ast.SelectorExpr && node.cond.or_block.kind != .absent)
			|| (node.cond is ast.CallExpr && node.cond.or_block.kind != .absent)
			|| (node.cond is ast.IndexExpr && node.cond.or_expr.kind != .absent)
		if cond_has_or_block {
			node.cond_type = node.cond_type.clear_flag(.option)
		}
		cond_is_option := node.cond_type.has_flag(.option)
		if (node.cond is ast.Ident && !cond_is_option)
			|| (node.cond is ast.SelectorExpr && node.cond.or_block.kind == .absent) {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			old_inside_opt_or_res := g.inside_opt_or_res
			if cond_is_option {
				g.inside_opt_or_res = true
			}
			g.expr(node.cond)
			g.inside_opt_or_res = old_inside_opt_or_res
			g.writeln(';')
		}
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		plus_plus_i := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${i}=builtin__overflow__add_i64(${i},1)'
			} $else {
				'${i}=builtin__overflow__add_i32(${i},1)'
			}
		} else {
			'++${i}'
		}
		g.empty_line = true
		opt_expr := '(*(${g.styp(node.cond_type.clear_flag(.option))}*)${cond_var}${op_field}data)'
		cond_expr := if cond_is_option {
			'${opt_expr}${op_field}len'
		} else {
			'${cond_var}${op_field}len'
		}
		g.writeln('for (${ast.int_type_name} ${i} = 0; ${i} < ${cond_expr}; ${plus_plus_i}) {')
		if node.val_var != '_' {
			if mut val_sym.info is ast.FnType {
				g.write('\t')
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
				g.writeln(' = ((voidptr*)${cond_var}${op_field}data)[${i}];')
			} else if !node.val_type.has_flag(.option) && val_sym.kind == .array_fixed
				&& !node.val_is_mut {
				right := '((${styp}*)${cond_var}${op_field}data)[${i}]'
				g.writeln('\t${styp} ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*(${styp}*)${c_name(node.val_var)}, (byte*)${right}, sizeof(${styp}));')
			} else {
				needs_memcpy := !node.val_type.is_ptr() && !node.val_type.has_flag(.option)
					&& g.table.final_sym(node.val_type).kind == .array_fixed
				right := if cond_is_option {
					'((${styp}*)${opt_expr}${op_field}data)[${i}]'
				} else if node.val_is_mut || node.val_is_ref {
					if g.table.value_type(node.cond_type).is_ptr() {
						'((${styp}*)${cond_var}${op_field}data)[${i}]'
					} else {
						'((${styp})${cond_var}${op_field}data) + ${i}'
					}
				} else if val_sym.kind == .array_fixed {
					'((${styp}*)${cond_var}${op_field}data)[${i}]'
				} else {
					'((${styp}*)${cond_var}${op_field}data)[${i}]'
				}
				if !needs_memcpy {
					g.writeln('\t${styp} ${c_name(node.val_var)} = ${right};')
				} else {
					g.writeln('\t${styp} ${c_name(node.val_var)} = {0};')
					g.writeln('\tmemcpy(${c_name(node.val_var)}, ${right}, sizeof(${styp}));')
				}
			}
		}
	} else if node.kind == .array_fixed {
		mut cond_var := ''
		cond_type_is_ptr := node.cond_type.is_ptr()
		cond_is_literal := node.cond is ast.ArrayInit
		if cond_is_literal {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			g.expr(node.cond)
			g.writeln(';')
		} else if cond_type_is_ptr {
			cond_var = g.new_tmp_var()
			cond_var_type := g.styp(node.cond_type).trim('*')
			if !node.cond.is_lvalue() {
				g.write('${cond_var_type} *${cond_var} = ((${cond_var_type})')
			} else {
				g.write('${cond_var_type} *${cond_var} = (')
			}
			g.expr(node.cond)
			g.writeln(');')
		} else {
			cond_var = g.expr_string(node.cond)
		}
		idx := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		plus_plus_idx := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${idx}=builtin__overflow__add_i64(${idx},1)'
			} $else {
				'${idx}=builtin__overflow__add_i32(${idx},1)'
			}
		} else {
			'++${idx}'
		}
		cond_sym := g.table.final_sym(node.cond_type)
		info := cond_sym.info as ast.ArrayFixed
		g.writeln('for (${ast.int_type_name} ${idx} = 0; ${idx} != ${info.size}; ${plus_plus_idx}) {')
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			is_fixed_array := val_sym.kind == .array_fixed && !node.val_is_mut
				&& !node.val_type.has_flag(.option)
			if val_sym.info is ast.FnType {
				g.write('\t')
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
			} else if is_fixed_array {
				styp := g.styp(node.val_type)
				g.writeln('\t${styp} ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*(${styp}*)${c_name(node.val_var)}, (byte*)${cond_var}[${idx}], sizeof(${styp}));')
			} else {
				styp := g.styp(node.val_type)
				g.write('\t${styp} ${c_name(node.val_var)}')
			}
			if !is_fixed_array {
				addr := if node.val_is_mut { '&' } else { '' }
				if cond_type_is_ptr {
					g.writeln(' = ${addr}(*${cond_var})[${idx}];')
				} else if cond_is_literal {
					g.writeln(' = ${addr}${cond_var}[${idx}];')
				} else {
					g.write(' = ${addr}')
					g.expr(node.cond)
					if info.is_fn_ret {
						g.write('.ret_arr')
					}
					g.writeln('[${idx}];')
				}
			}
		}
	} else if node.kind == .map {
		// `for key, val in map {
		// g.writeln('// FOR IN map')
		mut cond_var := ''
		if node.cond is ast.Ident {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		dot_or_ptr := if node.cond_type.has_flag(.shared_f) {
			'->val.'
		} else if node.cond_type.is_ptr() || resolved_cond_expr.is_auto_deref_var() {
			'->'
		} else {
			g.dot_or_ptr(node.cond_type)
		}
		idx := g.new_tmp_var()
		plus_plus_idx := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${idx}=builtin__overflow__add_i64(${idx},1)'
			} $else {
				'${idx}=builtin__overflow__add_i32(${idx},1)'
			}
		} else {
			'++${idx}'
		}
		map_len := g.new_tmp_var()
		g.empty_line = true
		g.writeln('${ast.int_type_name} ${map_len} = ${cond_var}${dot_or_ptr}key_values.len;')
		g.writeln('for (${ast.int_type_name} ${idx} = 0; ${idx} < ${map_len}; ${plus_plus_idx} ) {')
		// TODO: don't have this check when the map has no deleted elements
		g.indent++
		diff := g.new_tmp_var()
		g.writeln('${ast.int_type_name} ${diff} = ${cond_var}${dot_or_ptr}key_values.len - ${map_len};')
		g.writeln('${map_len} = ${cond_var}${dot_or_ptr}key_values.len;')
		// TODO: optimize this
		g.writeln('if (${diff} < 0) {')
		g.writeln('\t${idx} = -1;')
		g.writeln('\tcontinue;')
		g.writeln('}')
		g.writeln('if (!builtin__DenseArray_has_index(&${cond_var}${dot_or_ptr}key_values, ${idx})) {continue;}')
		if node.cond is ast.Ident {
			cond_ident := node.cond as ast.Ident
			resolved_key_type := g.resolve_current_fn_generic_param_key_type(cond_ident.name)
			if resolved_key_type != 0 {
				node.key_type = resolved_key_type
				if node.key_var.len > 0 {
					node.scope.update_var_type(node.key_var, node.key_type)
				}
			}
			resolved_val_type := g.resolve_current_fn_generic_param_value_type(cond_ident.name)
			if resolved_val_type != 0 {
				node.val_type = for_in_val_type(resolved_val_type, node.val_is_mut, node.val_is_ref)
				if node.val_var.len > 0 {
					node.scope.update_var_type(node.val_var, node.val_type)
				}
			}
		}
		if node.key_var != '_' {
			key_styp := g.styp(node.key_type)
			key := c_name(node.key_var)
			g.writeln('${key_styp} ${key} = *(${key_styp}*)builtin__DenseArray_key(&${cond_var}${dot_or_ptr}key_values, ${idx});')
			// TODO: analyze whether node.key_type has a .clone() method and call .clone() for all types:
			if node.key_type == ast.string_type {
				g.writeln('${key} = builtin__string_clone(${key});')
			}
		}
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			if val_sym.info is ast.FnType {
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
				g.write(' = (*(voidptr*)')
				g.writeln('builtin__DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}));')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				val_styp := g.styp(node.val_type)
				g.writeln('${val_styp} ${c_name(node.val_var)};')
				g.writeln('memcpy(*(${val_styp}*)${c_name(node.val_var)}, (byte*)builtin__DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}), sizeof(${val_styp}));')
			} else {
				val_styp := g.styp(node.val_type)
				if node.val_is_mut || node.val_is_ref {
					if g.table.value_type(node.cond_type).is_ptr() {
						g.write('${val_styp} ${c_name(node.val_var)} = (*(${val_styp}*)')
					} else {
						g.write('${val_styp} ${c_name(node.val_var)} = ((${val_styp})')
					}
				} else {
					g.write('${val_styp} ${c_name(node.val_var)} = (*(${val_styp}*)')
				}
				g.writeln('builtin__DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}));')
			}
		}
		g.indent--
	} else if node.kind == .string {
		cond := if node.cond in [ast.StringLiteral, ast.StringInterLiteral] {
			ast.Expr(g.new_ctemp_var_then_gen(node.cond, ast.string_type))
		} else {
			node.cond
		}
		field_accessor := if node.cond_type.is_ptr() { '->' } else { '.' }
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		plus_plus_i := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${i}=builtin__overflow__add_i64(${i},1)'
			} $else {
				'${i}=builtin__overflow__add_i32(${i},1)'
			}
		} else {
			'++${i}'
		}
		g.write('for (${ast.int_type_name} ${i} = 0; ${i} < ')
		g.expr(cond)
		g.writeln('${field_accessor}len; ${plus_plus_i}) {')
		if node.val_var != '_' {
			g.write('\tu8 ${c_name(node.val_var)} = ')
			g.expr(cond)
			g.writeln('${field_accessor}str[${i}];')
		}
	} else if node.kind in [.struct, .interface] {
		// In generic functions, `node.cond_type` may have been overwritten by the checker
		// for the last concrete specialization. Re-resolve from the function parameter's
		// declared type which still has the generic flag.
		mut unwrapped_cond_type := g.unwrap_generic(node.cond_type)
		if g.cur_concrete_types.len > 0 && g.cur_fn != unsafe { nil } && node.cond is ast.Ident {
			for param in g.cur_fn.params {
				if param.name == (node.cond as ast.Ident).name {
					resolved := g.unwrap_generic(param.typ)
					if resolved != unwrapped_cond_type {
						unwrapped_cond_type = resolved
					}
					break
				}
			}
		}
		cond_type_sym := g.table.sym(unwrapped_cond_type)
		mut next_fn := ast.Fn{}
		// use alias `next` method if exists else use parent type `next` method
		if cond_type_sym.kind == .alias {
			next_fn = cond_type_sym.find_method_with_generic_parent('next') or {
				g.table.final_sym(unwrapped_cond_type).find_method_with_generic_parent('next') or {
					verror('`next` method not found')
					return
				}
			}
		} else {
			next_fn = cond_type_sym.find_method_with_generic_parent('next') or {
				verror('`next` method not found')
				return
			}
		}
		ret_typ := g.unwrap_generic(next_fn.return_type)
		t_expr := g.new_tmp_var()
		g.write('${g.styp(unwrapped_cond_type)} ${t_expr} = ')
		g.expr(node.cond)
		g.writeln(';')
		i := node.key_var
		plus_plus_i := if g.do_int_overflow_checks {
			$if new_int ? && x64 {
				'${i}=builtin__overflow__add_i64(${i},1)'
			} $else {
				'${i}=builtin__overflow__add_i32(${i},1)'
			}
		} else {
			'++${i}'
		}
		if i in ['', '_'] {
			g.writeln('while (1) {')
		} else {
			g.writeln('for (size_t ${i} = 0;; ${plus_plus_i}) {')
		}
		t_var := g.new_tmp_var()
		receiver_typ := g.unwrap_generic(next_fn.params[0].typ)
		receiver_styp := g.cc_type(receiver_typ, false)
		mut fn_name := receiver_styp.replace_each(['*', '', '.', '__']) + '_next'
		receiver_sym := g.table.sym(receiver_typ)
		if receiver_sym.is_builtin() {
			fn_name = 'builtin__${fn_name}'
		} else if receiver_sym.info is ast.Interface {
			left_cc_type := g.cc_type(g.table.unaliased_type(unwrapped_cond_type), false)
			left_type_name := util.no_dots(left_cc_type)
			fn_name = '${c_name(left_type_name)}_name_table[${t_expr}._typ]._method_next'
		} else {
			fn_name = g.specialized_method_name_from_receiver(next_fn, unwrapped_cond_type, fn_name)
		}
		g.write('\t${g.styp(ret_typ)} ${t_var} = ${fn_name}(')
		if !node.cond_type.is_ptr() && receiver_typ.is_ptr() {
			g.write('&')
		}
		if node.kind == .interface {
			g.writeln('${t_expr}._object);')
		} else {
			g.writeln('${t_expr});')
		}
		g.writeln('\tif (${t_var}.state != 0) break;')
		val := if node.val_var in ['', '_'] { g.new_tmp_var() } else { node.val_var }
		val_styp := g.styp(ret_typ.clear_option_and_result())
		ret_sym := g.table.final_sym(ret_typ)
		if node.val_is_mut {
			if ret_typ.is_any_kind_of_pointer() {
				g.writeln('\t${val_styp} ${val} = *(${val_styp}*)${t_var}.data;')
			} else {
				g.writeln('\t${val_styp}* ${val} = (${val_styp}*)${t_var}.data;')
			}
		} else {
			ret_is_fixed_array := ret_sym.is_array_fixed()
			if ret_is_fixed_array {
				g.writeln('\t${val_styp} ${val} = {0};')
				g.write('\tmemcpy(${val}, ${t_var}.data, sizeof(${val_styp}));')
			} else {
				if ret_sym.info is ast.FnType {
					g.write_fntype_decl(val, ret_sym.info, 0)
					g.writeln(' = **(${val_styp}**)&${t_var}.data;')
				} else {
					g.writeln('\t${val_styp} ${val} = *(${val_styp}*)${t_var}.data;')
				}
			}
		}
	} else if node.kind == .aggregate {
		for_type := (g.table.sym(node.cond_type).info as ast.Aggregate).types[g.aggregate_type_idx]
		val_type := g.table.value_type(for_type)
		node.scope.update_var_type(node.val_var, val_type)

		g.for_in_stmt(ast.ForInStmt{
			cond:       node.cond
			cond_type:  for_type
			kind:       g.table.sym(for_type).kind
			stmts:      node.stmts
			val_type:   val_type
			val_var:    node.val_var
			val_is_mut: node.val_is_mut
			val_is_ref: node.val_is_ref
		})

		g.loop_depth--
		return
	} else {
		typ_str := g.table.type_to_str(node.cond_type)
		g.error('for in: unhandled symbol `${node.cond}` of type `${typ_str}`', node.pos)
	}
	g.write_labeled_continue_gate(node.label, '\t')
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}

	if node.kind == .map {
		// diff := g.new_tmp_var()
		// g.writeln('${ast.int_type_name} ${diff} = ${cond_var}${arw_or_pt}key_values.len - ${map_len};')
		// g.writeln('if (${diff} < 0) {')
		// g.writeln('\t${idx} = -1;')
		// g.writeln('\t${map_len} = ${cond_var}${arw_or_pt}key_values.len;')
		// g.writeln('}')
	}
	g.write_defer_stmts(node.scope, false, node.pos)
	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('\t${node.label}__break: {}')
	}
	g.loop_depth--
}
